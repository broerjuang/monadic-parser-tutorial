open BsBastet.Interface;

module Result = Relude.Result;

/**
  * Represent the current position
  * the current index of a string
**/
module Pos = {
  type t = int;
};

/**
  * Track the current position in specific string
**/
module PosString = {
  type t = {
    pos: Pos.t,
    str: string,
  };

  let make = (~pos, ~str) => {pos, str};
};

module ParseError = {
  type t =
    | ParseError(string);

  let make: string => t = s => ParseError(s);
  let show: t => string = (ParseError(str)) => str;
};

type success('a) = {
  result: 'a,
  suffix: PosString.t,
};

type error = {
  pos: Pos.t,
  error: ParseError.t,
};

module ResultE =
  Result.WithError({
    type t = error;
  });

type t('a) =
  | Parser(PosString.t => Result.t(success('a), error));

/**
  * Let's provide our own implementation of functor map for this Parser.
  * since it satisfy the law of Functor
  * map :: (a => b) => t('a) => t('b)
  * in our case,
  * map :: (a => b) => Parser('a) => Parser('b)
**/

let map = (fn, Parser(p)) =>
  Parser(
    posString =>
      p(posString) |> Result.map(res => {...res, result: fn(res.result)}),
  );

/**
  * Rather than recreating our own module type, which is interface of functor, just use bs-bastet directly
  * https://github.com/Risto-Stevcev/bastet/blob/master/bastet/src/Functions.re#L29
  * actually relude extension aliasing the Functor module (Higher order module) in Bastet
**/
module Functor: FUNCTOR with type t('a) = t('a) = {
  /**
    * nonrec is available on OCaml 4.02.2; in short every type definition is recursive unless it's told otherwise
    * https://caml.inria.fr/pub/docs/manual-ocaml/typedecl.html
  **/
  type nonrec t('a) = t('a);
  let map = map;
};

/**
  * so we got, https://github.com/Risto-Stevcev/bastet/blob/master/bastet/src/Functions.re#L30-L35
  * this extra for free (void, void_right, void_left, flap)
**/
include Relude.Extensions.Functor.FunctorExtensions(Functor);

/**
  * Let's now provide our own apply definition of our Parser
  * remember that apply signature
  * apply :: (t('a => 'b)) => t('a) => t('b)
**/

let apply = (Parser(pf), Parser(pa)) => {
  Parser(
    posString =>
      // apply the first parser, since it's a function
      pf(posString)
      // since the result is another parser, we need to flatMap the result
      |> Result.flatMap(({result: f, suffix: s1})
           // what we've got is that the result is still a function that wait another argument just like map
           // apply the result of first apply
           =>
             pa(s1)
             // flatMap it again
             |> Result.flatMap(({result: a, suffix: s2})
                  // and return the result
                  => Ok({result: f(a), suffix: s2}))
           ),
  );
};

/**
----------- apply using option ------------------------
if you're confused how that works. it's alright; let's take a look on the more simpler example
 that implement apply, an option
{[
  let square = (x) => {x * x};
  apply(Some(square), Some(12)) == Some(144);
  apply(Some(square), None) == None;
  apply(None, Some(12)) == None;
  apply(None, None) == None;
]}

from that we can infer that it only apply the function if both argument is Some. And if we look at the implementation, we may get

let apply = (optFn, opt) => {
 switch(optFn) {
 | Some(fn) => {
     switch(opt) {
      | Some(value) => Some(fn(value))
      | None => None
    }
   }
  | None => None
 }
}

or implement it using map

let apply = (optFn, opt) => {
  switch(optFn) {
    | Some(fn) => map(fn, opt)
    | None => None
  }
}

------------- Apply in Result ------------------------
let apply = (res_fn, res) => {
  switch(res_fn) {
    | Ok(fn) => map(fn, a)
    | Error(fn) => Error(fn)
  }
}

------------ Apply in List --------------------------
could you try this one?
**/
module Apply: APPLY with type t('a) = t('a) = {
  include Functor;
  let apply = apply;
};

include Relude.Extensions.Apply.ApplyExtensions(Apply);

/**
  * Sometimes, however, I forget that Applicative is an extension of type class apply, my bad.
  * what is the difference between apply and applicative lies in the existace of pure

  pure :: 'a => t('a)

  take expression and returned a box version of it, quite easy right?

  Quoted from http://eed3si9n.com/learning-scalaz/Applicative.html
  "pure should take a value of any type and return an applicative value with that value inside it. … A better way of thinking about pure would be to say that it takes a value and puts it in some sort of default (or pure) context—a minimal context that still yields that value."

  Let's implement Applicative type class then!
**/

let pure = a =>
  Parser(posString => Result.Ok({result: a, suffix: posString}));

/** and put it into applicative instance **/
module Applicative: APPLICATIVE with type t('a) = t('a) = {
  include Apply;
  let pure = pure;
};

include Relude.Extensions.Applicative.ApplicativeExtensions(Applicative);

/**
  * Let's implement alt type class
  * for me it's type class that define alternative, from left to right
  * if first one is failed, try the second, if still fail; let it fail

  --------------
  * ups, I forget to mention about infix function.
  * Let's start:
  * functor : map has an <$> operator
  * Apply: apply has <*>
  * Alt: alt has <|> (just like or, right)
  -------------
  Again, with alt :

  so alt has this signature: t('a) => t('a) => t('a)

**/

let alt = (Parser(p1), Parser(p2)) => {
  Parser(
    posString => {
      switch (p1(posString)) {
      | Ok(_) as ok => ok
      | Error({pos}) as e =>
        if (posString.pos == pos) {
          p2(posString);
        } else {
          e;
        }
      }
    },
  );
};

/**
  * Let's include alternative type class
**/
module Alt: ALT with type t('a) = t('a) = {
  include Functor;
  let alt = alt;
};

include Relude.Extensions.Alt.AltExtensions(Alt);

/**
  * last but not least, bind
  * it's useful if you want to sequencing parsers
  * bind of flatMap has a signature:
  bind :: t('a) => ('a => t('b)) => t('b)
  and the infix operator for this is >>=

  if, again, the signature seems strange, it's alright. Let's take a look at the implementation inside the option

  let bind = (optA, aToOptB) => {
    switch(optA) {
     | Some(a) => aToOptB(a)
     | None => None
    }
  }

  in other words, it will apply if the first one is Some and will return the boxed version of the transformation

  let's take a look of our implementation of bind
**/

let bind = (Parser(pa), aToPB) => {
  Parser(
    posString => {
      pa(posString)
      |> Result.flatMap(({result, suffix}) => {
           let Parser(pb) = aToPB(result);
           pb(suffix);
         })
    },
  );
};

/** then we will create a monad instance from this **/
module Monad: MONAD with type t('a) = t('a) = {
  include Applicative;
  let flat_map = bind;
};

include Relude.Extensions.Monad.MonadExtensions(Monad);
