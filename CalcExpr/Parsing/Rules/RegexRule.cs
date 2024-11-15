using CalcExpr.Expressions;
using CalcExpr.Tokenization.Tokens;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;
using System.Text.RegularExpressions;

namespace CalcExpr.Parsing.Rules;

public class RegexRule(string name, [StringSyntax(StringSyntaxAttribute.Regex)] string regex,
    Func<ImmutableArray<IToken>, TokenMatch, Parser, IExpression> parse, RegexOptions options = RegexOptions.None)
    : IParserRule
{
    private readonly Regex _regex = new Regex(regex, options);
    private readonly Func<ImmutableArray<IToken>, TokenMatch, Parser, IExpression> _parse = parse;

    public string Name { get; } = name;

    public string Regex { get; } = regex;

    public RegexOptions Options { get; } = options;

    public TokenMatch? Match(ImmutableArray<IToken> input, IEnumerable<IParserRule> _)
    {
        string inputString = string.Join("", input.Select(x => x.RegexAlias));
        Match match = _regex.Match(inputString);

        if (match.Success)
            return new TokenMatch(input[match.Index..(match.Index + match.Length)], match.Index);

        return null;
    }

    public IExpression? Parse(ImmutableArray<IToken> input, Parser parser)
    {
        TokenMatch? match = Match(input, parser.Grammar);

        if (match is not null)
            return Parse(input, match, parser);

        return null;
    }

    public IExpression? Parse(ImmutableArray<IToken> input, TokenMatch match, Parser parser)
        => _parse(input, match, parser);
}
