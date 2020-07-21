module.exports = grammar({
  name: 'ql',

  conflicts: $ => [
    [$.simpleId, $.className],
    [$.simpleId, $.literalId],
  ],

  extras: $ => [
    /[ \t\r\n]/,
    $.line_comment,
    $.block_comment,
  ],

  supertypes: $ => [
    $._primary,
    $._exprOrTerm,
    $._moduleMember,
    $._typeExpr,
    $._literal
  ],

  word: $ => $._lower_id,

  rules: {
    ql: $ => repeat(field("member", $._moduleElement)),

    module: $ => seq(
      'module',
      field("name", $.moduleName),
      field("body", choice(
        $.moduleBody,
        $.moduleAliasBody
      ))
    ),

    moduleBody: $ => seq(
      "{",
      repeat(field("body", $._moduleElement)),
      "}"
    ),

    _moduleElement: $ => choice(
      $.annotatedModuleMember,
      $.qldoc
    ),

    annotatedModuleMember: $ => seq(
      repeat(field("annotation", $.annotation)),
      field("member", $._moduleMember)
    ),

    _moduleMember: $ => choice($.importDirective, $.classlessPredicate, $.dataclass, $.datatype, $.select, $.module),

    importDirective: $ => seq(
      'import',
      field("moduleExpr", $.importModuleExpr),
      optional(seq('as', field("name", $.moduleName)))
    ),

    moduleAliasBody: $ => seq($.eq, field("moduleExpr", $.moduleExpr), ";"),
    predicateAliasBody: $ => seq($.eq, field("predicateExpr", $.predicateExpr), ";"),
    typeAliasBody: $ => seq($.eq, field("type", $._typeExpr), ";"),
    typeUnionBody: $ => seq($.eq, field("type", $._typeExpr), "or", sep(field("type", $._typeExpr), "or"), ";"),

    classlessPredicate: $ => seq(
      field("returnType", choice($.predicate, $._typeExpr)),
      field("name", $.predicateName),
      choice(
        seq("(", sep(field("varDecl", $.varDecl), ","), ")", field("body", $._optbody)),
        field("body", $.predicateAliasBody)
      )
    ),
  
    datatype: $ => seq(
      'newtype',
      field("name", $.className),
      $.eq,
      $._datatypeBranches
    ),

    _datatypeBranches: $ => sep1(field("branch", $.datatypeBranch), "or"),

    datatypeBranch: $ => seq(
      optional(field("qldoc", $.qldoc)),
      optional(field("annotation", $.annotation)),
      field("name", $.className),
      "(",
      sep(field("varDecl", $.varDecl), ","),
      ")",
      optional(field("body", $.body))
    ),

    select: $ => seq(
      optional(seq("from", sep(field("varDecl", $.varDecl), ","))),
      optional(seq("where", field("where", $._exprOrTerm))),
      seq('select', sep1(field("select", $.asExpr), ",")),
      optional($._orderBys)
    ),

    dataclass: $ => seq(
      'class',
      field("name", $.className),
      field("body", choice(
        $.dataclassBody,
        $.typeAliasBody,
        $.typeUnionBody
      ))
    ),

    dataclassBody: $ => seq('extends', sep1(field("baseType", $._typeExpr), ","), "{", repeat(field("member", $._classMember)), "}"),

    _classMember: $ => choice(
      $.classMember,
      $.qldoc
    ),

    classMember: $ => seq(
      repeat(field("annotation", $.annotation)),
      field("member", choice($.charpred, $.memberPredicate, $.field))
    ),

    charpred: $ => seq(field("name", $.className), "(", ")", "{", field("body", $._exprOrTerm), "}"),

    memberPredicate: $ => seq(
      field("returnType", choice($.predicate, $._typeExpr)),
      field("name", $.predicateName),
      "(",
      sep(field("varDecl", $.varDecl), ","),
      ")",
      $._optbody
    ),

    field: $ => seq(field("varDecl", $.varDecl), ";"),

    _optbody: $ => choice(
      $.empty,
      $.body,
      $.higherOrderTerm
    ),

    empty: $ => ";",

    body: $ => seq("{", field("term", $._exprOrTerm), "}"),

    higherOrderTerm: $ => seq(
      $.eq,
      field("name", $.literalId),
      "(",
      sep(field("predicateExpr", $.predicateExpr), ","),
      ")",
      "(",
      sep(field("arg", $._call_arg), ","),
      ")"
    ),

    special_call: $ => seq(field("id", $.specialId), "(", ")"),
    prefix_cast: $ => prec.dynamic(10, seq("(", field("type", $._typeExpr), ")", field("expr", $._exprOrTerm))),
    unary_expr: $ => seq(field("operator", $.unop), field("expr", $._exprOrTerm)),
    binary_expr: $ => choice(
      $._mul_expr,
      $._add_expr,
    ),
    _mul_expr: $ => prec.left(9, seq(
      field('left', $._exprOrTerm),
      field('operator', $._mulop),
      field('right', $._exprOrTerm)
    )),
    _add_expr: $ => prec.left(8, seq(
      field('left', $._exprOrTerm),
      field('operator', $._addop),
      field('right', $._exprOrTerm)
    )),
    in_expr: $ => prec.left(7, seq(
      field('left', $._exprOrTerm),
      'in',
      field('right', $.range)
    )),
    comp_term: $ => prec.left(6, seq(
      field('left', $._exprOrTerm),
      field('operator', $._compop),
      field('right', $._exprOrTerm)
    )),
    instance_of: $ => prec.left(5, seq(
      field('expr', $._exprOrTerm),
      'instanceof',
      field('type', $._typeExpr)
    )),
    negation: $ => prec.left(4, seq('not', field('expr', $._exprOrTerm))),
    if_term: $ => prec.left(3, seq(
      "if", field('cond', $._exprOrTerm),
      "then", field('first', $._exprOrTerm),
      "else", field('second', $._exprOrTerm)
    )),
    conjunction: $ => prec.left(3, seq(
      field('left', $._exprOrTerm),
      "and",
      field('right', $._exprOrTerm)
    )),
    disjunction: $ => prec.left(2, seq(
      field('left', $._exprOrTerm),
      "or",
      field('right', $._exprOrTerm)
    )),
    implication: $ => prec.left(1, seq(
      field('left', $._exprOrTerm),
      "implies",
      field('right', $._exprOrTerm)
    )),

    quantified: $ => seq(
      field("quantifier", $._quantifier),
      "(",
      choice(
        seq(
          sep(field("varDecl", $.varDecl), ","),
          optional(seq("|", field("term", $._exprOrTerm), optional(seq("|", field("term", $._exprOrTerm)))))
        ),
        field("term", $._exprOrTerm)
      ),
      ")"),
    
    specialId: $ => 'none',

    _quantifier: $ => choice('exists', 'forall', 'forex'),

    _call_arg: $ => choice(
      $._exprOrTerm,  // ExprArg
      $.underscore  // DontCare
    ),

    qualified_call: $ => seq( // QualCall
      field("name", $.predicateName),
      optional(field("closure", $.closure)),
      "(",
      sep(field("arg", $._call_arg), ","),
      ")"
    ),
  
    qualified_cast: $ => seq( // QualCast
      "(",
      field("type", $._typeExpr),
      ")"
    ),

    _qualifiedRhs: $ => choice(
      $.qualified_call,
      $.qualified_cast
    ),

    call_body:$ => seq("(", sep(field("arg", $._call_arg), ","), ")"),
    unqual_agg_body:$ => seq("(",  sep($.varDecl, ","), "|", optional($._exprOrTerm), optional(seq("|", $.asExprs)), ")"),

    _call_or_unqual_agg_body: $ => choice($.call_body, $.unqual_agg_body),

    call_or_unqual_agg_expr: $ => prec.dynamic(10, seq(
      field("predicate", $.aritylessPredicateExpr),
      optional(field("closure", $.closure)),
      field("body", $._call_or_unqual_agg_body)
    )),

    qualified_expr: $ => seq(field("lhs", $._primary), ".", field("rhs", $._qualifiedRhs)),
    super_ref: $ => seq(optional(seq(field("type", $._typeExpr), ".")), $.super),


    // The split here is to ensure that the node is non-empty
    full_aggregate_body: $ => choice(
      seq(sep($.varDecl, ","),
        seq(
          "|",
          optional($._exprOrTerm),
          optional(seq("|", $.asExprs, optional($._orderBys)))
        )
      ),
      sep1($.varDecl, ","),
      ),

    expr_aggregate_body: $ => seq($.asExprs, optional($._orderBys)),

    aggregate: $ => seq(
      field("name", $.aggId),                                                                // Agg
      optional(
        seq("[", sep1(field("rank", $._exprOrTerm), ","), "]")
      ),
      "(",
      optional(
        field("body", choice($.full_aggregate_body, $.expr_aggregate_body))
      ),
      ")"
    ),
    range: $ => seq(                                                                        // Range
      "[",
      field('lower', $._exprOrTerm), "..", field('upper', $._exprOrTerm),
      "]"
    ),
    set_literal: $ => seq(
      "[",
      sep(field("expr", $._exprOrTerm), ','),
      "]"
    ),

    par_expr: $ => seq("(", field("expr", $._exprOrTerm), ")"),


    _exprOrTerm: $ => choice(
      $.special_call,
      $.prefix_cast,
      $._primary,
      $.unary_expr,
      $.binary_expr,
      $.in_expr,
      $.comp_term,
      $.instance_of,
      $.negation,
      $.if_term,
      $.conjunction,
      $.disjunction,
      $.implication,
      $.quantified,                         // QuantifiedTerm
    ),

    _primary: $ => choice(
      $.call_or_unqual_agg_expr, //
      $.qualified_expr,                                        // QualifiedExpr
      $._literal,                                                                  // Lit
      $.variable,                                                                 // Var
      $.super_ref,
      $.aggregate,
      $.range,
      $.set_literal,
      $.par_expr                                                 // ParExpr
    ),

    _literal: $ => choice(
      $.integer,     // IntLit
      $.float,       // FloatLit
      $.bool,        // BoolLit
      $.string       // StringLit
    ),


    bool: $ => choice($.true, $.false),

    variable: $ => choice($.this, $.result, $.varName),

    _compop: $ => choice($.eq, $.ne, $.lt, $.gt, $.le, $.ge),

    unop: $ => choice($.plus, $.minus),

    _mulop: $ => choice($.star, $.slash, $.mod),

    _addop: $ => choice($.plus, $.minus),

    closure: $ => choice($.star, $.plus),

    direction: $ => choice('asc', 'desc'),

    varDecl: $ => seq(field("type", $._typeExpr), field("name", $.varName)),

    asExprs: $ => sep1($.asExpr, ","),

    asExpr: $ => seq($._exprOrTerm, optional(seq('as', $.varName))),

    _orderBys: $ => seq("order", "by", sep1(field("orderBy", $.orderBy), ",")),

    orderBy: $ => seq($._exprOrTerm, optional($.direction)),

    qldoc: $ => /\/\*\*[^*]*\*+([^/*][^*]*\*+)*\//,

    literalId: $ => choice($._lower_id, $._upper_id),

    annotation: $ => choice(
      field('name', $.annotName), // SimpleAnnotation
      seq( // ArgsAnnotation
        field('name', $.annotName),
        "[",
        sep1(field('arg', $.annotArg), ","),
        "]"
      )
    ),

    annotName: $ => $._lower_id,

    annotArg: $ => choice($.simpleId, $.this, $.result),

    moduleName: $ => field("name", $.simpleId),

    qualModuleExpr: $ => sep1(field("name", $.simpleId), "."),

    importModuleExpr: $ => seq(field("qualifier", $.qualModuleExpr), repeat(seq("::", field("name", $.simpleId)))),

    moduleExpr: $ => choice(
      field("name", $.simpleId),
      seq(field("qualifier", $.moduleExpr), "::", field("name", $.simpleId))
    ),

    primitiveType: $ => choice('boolean', 'date', 'float', 'int', 'string'),

    simpleId: $ => choice($._lower_id, $._upper_id),

    className: $ => $._upper_id,

    dbtype: $ => /@[a-z][A-Za-z0-9_]*/,

    namedTypeExpr: $ => seq(optional(seq(field("qualifier", $.moduleExpr), "::")), field("name", $.className)),

    _typeExpr: $ => choice(
      $.namedTypeExpr,
      $.dbtype,
      $.primitiveType
    ),

    predicateName: $ => $._lower_id,

    aritylessPredicateExpr: $ => seq(optional(seq(field("qualifier", $.moduleExpr), "::")), field("name", $.literalId)),

    predicateExpr: $ => seq(field("qualifiedName", $.aritylessPredicateExpr), $.slash, field("arity", $.integer)),

    varName: $ => $.simpleId,

    aggId: $ => choice('avg', 'concat', 'strictconcat', 'count', 'max', 'min', 'rank', 'strictcount', 'strictsum', 'sum', 'any'),

    _upper_id: $ => /[A-Z][A-Za-z0-9_]*/,
    _lower_id: $ => /[a-z][A-Za-z0-9_]*/,
    integer: $ => /[0-9]+/,
    float: $ => /[0-9]+\.[0-9]+/,
    string: $ => /"([^"\\\r\n\t]|\\["\\nrt])*"/,
    line_comment: $ => /\/\/[^\r\n]*/,
    block_comment: $ => /\/\*([^*]+\*+([^/*][^*]*\*+)*|\*)\//,

    false: $ => 'false',
    predicate: $ => 'predicate',
    result: $ => 'result',
    super: $ => 'super',
    this: $ => 'this',
    true: $ => 'true',

    // symbols
    lt: $ => '<',
    le: $ => '<=',
    eq: $ => '=',
    gt: $ => '>',
    ge: $ => '>=',
    underscore: $ => '_',
    minus: $ => '-',
    ne: $ => '!=',
    slash: $ => '/',
    star: $ => '*',
    mod: $ => '%',
    plus: $ => '+',
  }
});

function sep(rule, s) {
  return optional(sep1(rule, s))
}

function sep1(rule, s) {
  return seq(rule, repeat(seq(s, rule)))
}
