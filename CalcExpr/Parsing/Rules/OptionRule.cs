using CalcExpr.Expressions;
using CalcExpr.Tokenization.Tokens;
using System.Collections.Immutable;

namespace CalcExpr.Parsing.Rules;

public class OptionRule(string name, string[] options, Func<IToken, Parser, IExpression> parse,
    StringComparison stringComparison = StringComparison.CurrentCulture)
    : IParserRule
{
    private readonly Func<IToken, Parser, IExpression> _parse = parse;

    public string Name { get; } = name;

    public readonly ImmutableHashSet<string> Options = options.Distinct().ToImmutableHashSet();

    public readonly StringComparison Comparison = stringComparison;

    public TokenMatch? Match(ImmutableArray<IToken> input, IEnumerable<IParserRule> _)
    {
        if (input.Length == 1 && Options.Contains(input.First().Value))
            return new TokenMatch([input.First()], 0);

        return null;
    }

    public IExpression? Parse(ImmutableArray<IToken> input, Parser parser)
    {
        if (input.Length == 1 && Options.Contains(input.First().Value))
            return _parse(input.First(), parser);

        return null;
    }

    public IExpression? Parse(ImmutableArray<IToken> _, TokenMatch match, Parser parser)
        => Parse([.. match.Match], parser);
}
