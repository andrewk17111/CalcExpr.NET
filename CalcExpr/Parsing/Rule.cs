using CalcExpr.Expressions;

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
}
