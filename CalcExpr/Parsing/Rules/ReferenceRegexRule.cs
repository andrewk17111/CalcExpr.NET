using CalcExpr.Attributes;
using CalcExpr.Expressions;
using System.Text.RegularExpressions;

namespace CalcExpr.Parsing.Rules;

[ReferenceRule]
public class ReferenceRegexRule : NestedRegexRule
{
    /// <summary>
    /// A rule to be used as a reference for a <see cref="NestedRegexRule"/>.
    /// </summary>
    /// <param name="name">The name of the <see cref="ReferenceRegexRule"/>.</param>
    /// <param name="regex">The regex <see cref="string"/> to match an expression <see cref="string"/> to.</param>
    public ReferenceRegexRule(string name, string regex)
        : base(name, regex, RegexOptions.None, (_, _, _) => Constant.UNDEFINED)
    { }

    public override bool Equals(object? obj)
        => obj is not null && obj is RegexRule a && RegularExpression == a.RegularExpression;

    public override int GetHashCode()
        => RegularExpression.GetHashCode();

    public static bool operator ==(ReferenceRegexRule a, ReferenceRegexRule b)
        => a.Equals(b);

    public static bool operator !=(ReferenceRegexRule a, ReferenceRegexRule b)
        => !a.Equals(b);
}
