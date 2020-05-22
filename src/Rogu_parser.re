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
