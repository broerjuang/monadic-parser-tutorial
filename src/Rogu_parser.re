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
