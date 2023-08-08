using CalcExpr.Expressions;
using System.Diagnostics.CodeAnalysis;

namespace CalcExpr.Parsing;

public readonly struct Rule
{
    public readonly string RegularExpression;
    public readonly Func<string, Token, IExpression> Parse;

    public Rule(string regex, Func<string, Token, IExpression> parse)
    {
        RegularExpression = regex;
        Parse = parse;
    }

    public override bool Equals([NotNullWhen(true)] object? obj)
        => throw new NotImplementedException();

    public override int GetHashCode()
        => throw new NotImplementedException();

    public static bool operator ==(Rule a, Rule b)
        => a.Equals(b);

    public static bool operator !=(Rule a, Rule b)
        => !a.Equals(b);
}
