using CalcExpr.Expressions;
using CalcExpr.Tokenization.Tokens;
using System.Collections.Immutable;

namespace CalcExpr.Parsing.Rules;

/// <summary>
/// A rule to be used to parse a <see cref="IEnumerable{IToken}"/> into an <see cref="IExpression"/>.
/// </summary>
/// <param name="name">The name of the <see cref="IParserRule"/>.</param>
/// <param name="parse">The function to use to parse a input <see cref="IEnumerable{IToken}"/>.</param>
/// <param name="match">The function to use to find a match in the input <see cref="IEnumerable{IToken}"/>.</param>
public class ParserRule(string name, Func<ImmutableArray<IToken>, Parser, IExpression?> parse,
    Func<ImmutableArray<IToken>, IEnumerable<IParserRule>, TokenMatch?> match,
    Func<ImmutableArray<IToken>, TokenMatch, Parser, IExpression>? parseMatch = null) : IParserRule
{
    private readonly Func<ImmutableArray<IToken>, Parser, IExpression?> _parse = parse;
    private readonly Func<ImmutableArray<IToken>, IEnumerable<IParserRule>, TokenMatch?> _match = match;
    private readonly Func<ImmutableArray<IToken>, TokenMatch, Parser, IExpression>? _parseMatch = parseMatch;

    public string Name { get; } = name;

    public ParserRule(string name, Func<ImmutableArray<IToken>, TokenMatch, Parser, IExpression> parse,
        Func<ImmutableArray<IToken>, IEnumerable<IParserRule>, TokenMatch?> match)
        : this(name, MatchAndParse(parse, match), match, parse)
    {}

    public TokenMatch? Match(ImmutableArray<IToken> input, IEnumerable<IParserRule> rules)
        => _match(input, rules);

    public IExpression? Parse(ImmutableArray<IToken> input, Parser parser)
        => _parse(input, parser);

    public IExpression? Parse(ImmutableArray<IToken> input, TokenMatch match, Parser parser)
        => _parseMatch is null ? Parse(input, parser) : _parseMatch(input, match, parser);

    private static Func<ImmutableArray<IToken>, Parser, IExpression?> MatchAndParse(
        Func<ImmutableArray<IToken>, TokenMatch, Parser, IExpression> parse,
        Func<ImmutableArray<IToken>, IEnumerable<IParserRule>, TokenMatch?> match)
    {
        return (i, p) => {
            TokenMatch? foundMatch = match(i, p.Grammar);

            return foundMatch is not null
                ? parse(i, foundMatch, p)
                : null;
        };
    }

    public override bool Equals(object? obj)
        => obj is not null && obj is ParserRule r && r._parse == _parse && r._match == _match &&
            r._parseMatch == _parseMatch;

    public override int GetHashCode()
        => HashCode.Combine(Name, _parse, _match, _parseMatch);

    public static bool operator ==(ParserRule a, ParserRule b)
        => a.Equals(b);

    public static bool operator !=(ParserRule a, ParserRule b)
        => !a.Equals(b);
}

public interface IParserRule
{
    public string Name { get; }

    /// <summary>
    /// The function that gets used to find a match in an input.
    /// </summary>
    /// <param name="input">The input <see cref="ImmutableArray{IToken}"/> to find the match in.</param>
    /// <param name="rules">Other rules from the calling <see cref="Parser"/>.</param>
    /// <returns>
    /// A new <see cref="IEnumerable{IToken}"/> containing the value of the matching input and the index of where it
    /// was found; <see langword="null"/> if no match was found.
    /// </returns>
    TokenMatch? Match(ImmutableArray<IToken> input, IEnumerable<IParserRule> rules);

    /// <summary>
    /// Tries to parse the input <see cref="string"/> into an <see cref="IExpression"/>.
    /// </summary>
    /// <param name="input">The input <see cref="ImmutableArray{IToken}"/> to parse.</param>
    /// <param name="parser">The <see cref="Parser"/> to use to parse any sub-expressions.</param>
    /// <returns>
    /// The parsed <see cref="IExpression"/>; <see langword="null"/> if the input could not be parsed.
    /// </returns>
    IExpression? Parse(ImmutableArray<IToken> input, Parser parser);

    /// <summary>
    /// Tries to parse the input <see cref="ImmutableArray{IToken}"/> into an <see cref="IExpression"/> using the given token.
    /// </summary>
    /// <param name="input">The input <see cref="ImmutableArray{IToken}"/> to parse.</param>
    /// <param name="match">The matching <see cref="IEnumerable{IToken}"/> with its index to use to parse the input.</param>
    /// <param name="parser">The <see cref="Parser"/> to use to parse any sub-expressions.</param>
    /// <returns>
    /// The parsed <see cref="IExpression"/>; <see langword="null"/> if the input could not be parsed.
    /// </returns>
    IExpression? Parse(ImmutableArray<IToken> input, TokenMatch match, Parser parser);
}
