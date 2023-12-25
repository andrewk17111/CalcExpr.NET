namespace CalcExpr.Exceptions;

/// <summary>
/// Initializes a new instance of the <see cref="NonRegexRuleException"/> class.
/// </summary>
/// <param name="name">The name of the invalid <see cref="Rule"/> being referenced.</param>
/// <param name="type">The type of the invalid <see cref="Rule"/> being referenced.</param>
public class NonRegexRuleException(string name, Type type)
    : Exception($"The referenced rule '{name}' is of type '{type}' and not a derivative of RegexRule.")
{ }
