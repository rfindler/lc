#lang scribble/manual

@title{LC, a language for solving exercises 3.16 and 3.17 in SeWPR}

@defmodulelang[lc]

This language provides an implementation of the λ-calculus
plus a way to define shorthands for specific lambda terms
and some support for testing.

Expressions at the top-level of the language print their
results, either as the free variable that they reduce to or,
if the lambda term is a church-encoded number or boolean,
the corresponding natural number or boolean is printed.

The only constants allowed are natural numbers, @racket[#true], and
@racket[#false], which represent their Church encodings.

@defform[(λ (id ...+) body-expr)]{
 A lambda expression; if there is more than one argument
 supplied, the lambda is curried.
}

@defform[#:id #%app (function-expr argument-expr ...+)]{
  Evaluates @racket[function-expr], checks that it
 is the result of a λ expression and then invokes it,
 passing the first of the @racket[argument-expr] as its
 argument.

 If more than one @racket[argument-expr] is supplied,
 the result of the first application is expected to be
 another λ expression and it invoked. Or, in other words,
 @racketblock[(function-expr _argument-expr_1 _argument-expr_2)]
 is the same thing as
 @racketblock[((function-expr _argument-expr_1) _argument-expr_2)]
 and ditto for subsequent arguments.
}

@defform[(define id expr)]{
 Introduces a shorthand for @racket[id], binding it to the
 λ-term @racket[expr]
}

@defform[(= expr expr)]{

 Checks to see that the two expressions are both
 Church-encoded numbers and that they are the same.

}

@defform*[[(assert expr)
           (assert ¬ expr)
           (assert not expr)]]{
 Checks to see if @racket[expr] is a church-encoded boolean.

 In the first case, makes sure it is true and the second and third cases,
 makes sure it is false.
}
