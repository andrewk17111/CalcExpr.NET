using CalcExpr.Expressions;
using System.Diagnostics.CodeAnalysis;

namespace CalcExpr.Parsing;

public readonly struct Rule
{
    public readonly string RegularExpression;
    public readonly Func<string, Token, IExpression> Parse;

    /// <summary>
    /// A rule to be used to parse a <see cref="string"/> into an <see cref="IExpression"/>.
    /// </summary>
    /// <param name="regex">The regex <see cref="string"/> to match an expression <see cref="string"/> to.</param>
    /// <param name="parse">The function to use to parse a <see cref="string"/>.</param>
    public Rule(string regex, Func<string, Token, IExpression> parse)
    {
        RegularExpression = regex;
        Parse = parse;
    }

    public override bool Equals(object? obj)
        => obj is not null && obj is Rule a && RegularExpression == a.RegularExpression;

    public override int GetHashCode()
        => RegularExpression.GetHashCode();

    public static bool operator ==(Rule a, Rule b)
        => a.Equals(b);

    public static bool operator !=(Rule a, Rule b)
        => !a.Equals(b);
}
