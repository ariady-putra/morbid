//// # Lazy Short-circuit Validations
////
//// Chain together many validations that may fail.
////
//// For example,
//// ```
//// validator {
////   fn validate(datum: Data, redeemer: Data, context: ScriptContext) -> Bool {
////     chain.validate()
////       |> and(must_be_condition_1(_, context, datum))
////       |> and(must_be_condition_2(_, context, redeemer))
////       |> and(must_be_condition_3(_, context))
////   }
//// }
//// ```
//// Where,
//// ```
//// fn must_be_condition_1(_, context, datum) {
////   let condition_1 = context |> must_validate_datum(datum) // call another fn somewhere
////   condition_1? // useful for tracing
//// }
//// 
//// fn must_be_condition_2(_, context, redeemer) {
////   let condition_2 = context |> must_validate_redeemer(redeemer) // call another fn somewhere
////   condition_2? // useful for tracing
//// }
//// 
//// fn must_be_condition_3(_, context) {
////   let condition_3 = context |> must_validate_other_things() // call another fn somewhere
////   condition_3? // useful for tracing
//// }
//// ```
//// As of `Aiken v1.0.10-alpha` it's not possible to directly chain `|>` with `?` and `&&` operators,
//// for example:
//// ```
//// (context |> must_be_condition_1())? && (context |> must_be_condition_2())? && (context |> must_be_condition_3())?
//// ```
//// Calling `|>` indirectly by declaring anonymous functions could give weird non-short-circuit behavior.
//// Using `list.and([fn(..) {..}])` or `reduce` or `foldr` and `foldr` loses the lazyness behavior for some reason.
//// Hence this solution; internally it mimics the `option.and_then` function.

pub fn validate() {
  True
}

pub fn and(continue, next) {
  if continue {
    next(True)
  } else {
    False
  }
}
