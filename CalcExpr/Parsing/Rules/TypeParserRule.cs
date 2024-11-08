using CalcExpr.Expressions;
using CalcExpr.Tokenization.Tokens;

namespace CalcExpr.Parsing.Rules;

public class TypeParserRule<T>(string name, Func<T, Parser, IExpression> parse) : IParserRule
    where T : IToken
{
    public string Name { get; } = name;

    public TokenMatch? Match(List<IToken> input, IEnumerable<IParserRule> rules)
    {
        if (input.Count == 1 && input.First() is T)
            return new TokenMatch([input.First()], 0);

        return null;
    }

    public IExpression? Parse(List<IToken> input, Parser parser)
    {
        if (input.Count == 1 && input.First() is T token)
            return parse(token, parser);

        return null;
    }

    public IExpression? Parse(List<IToken> input, TokenMatch match, Parser parser)
        => Parse([.. match.Match], parser);
}
