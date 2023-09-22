namespace CalcExpr.Exceptions;

public class NonRegexRuleException : Exception
{
    /// <summary>
    /// Initializes a new instance of the <see cref="NonRegexRuleException"/> class.
    /// </summary>
    /// <param name="name">The name of the invalid <see cref="Rule"/> being referenced.</param>
    /// <param name="type">The type of the invalid <see cref="Rule"/> being referenced.</param>
    public NonRegexRuleException(string name, Type type)
        : base($"The referenced rule '{name}' is of type '{type}' and not a derivative of RegexRule.")
    { }
}
