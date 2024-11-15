using CalcExpr.Expressions;
using CalcExpr.Tokenization.Tokens;
using System.Collections.Immutable;

namespace CalcExpr.Parsing.Rules;

public class TypeRule<T>(string name, Func<T, Parser, IExpression> parse) : IParserRule
    where T : IToken
{
    private readonly Func<T, Parser, IExpression> _parse = parse;

    public string Name { get; } = name;

    public TokenMatch? Match(ImmutableArray<IToken> input, IEnumerable<IParserRule> rules)
    {
        if (input.Length == 1 && input.First() is T)
            return new TokenMatch([input.First()], 0);

        return null;
    }

    public IExpression? Parse(ImmutableArray<IToken> input, Parser parser)
    {
        if (input.Length == 1 && input.First() is T token)
            return _parse(token, parser);

        return null;
    }

    public IExpression? Parse(ImmutableArray<IToken> _, TokenMatch match, Parser parser)
        => Parse([.. match.Match], parser);
}
