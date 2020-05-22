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

module Apply: APPLY with type t('a) = t('a) = {
  include Functor;
  let apply = apply;
};

include Relude.Extensions.Apply.ApplyExtensions(Apply);

