using CalcExpr.Expressions;

namespace CalcExpr.Parsing.Rules;

/// <summary>
/// A rule to be used to parse a <see cref="string"/> into an <see cref="IExpression"/>.
/// </summary>
/// <param name="name">The name of the <see cref="Rule"/>.</param>
/// <param name="parse">The function to use to parse a input <see cref="string"/>.</param>
/// <param name="match">The function to use to find a match in the input <see cref="string"/>.</param>
public class Rule(string name, Func<string, Token, Parser, IExpression> parse,
    Func<string, IEnumerable<Rule>, Token?> match)
{
    private readonly Func<string, IEnumerable<Rule>, Token?> _match = match;

    public readonly string Name = name;
    public readonly Func<string, Token, Parser, IExpression> Parse = parse;

    /// <summary>
    /// The function that gets used to find a match in an input <see cref="string"/>. This function might get overriden
    /// in derrived classes.
    /// </summary>
    /// <param name="input">The input <see cref="string"/> to find the match in.</param>
    /// <param name="rules">Other rules from the calling <see cref="Parser"/>.</param>
    /// <returns>
    /// A new <see cref="Token"/> containing the value of the matching <see cref="string"/> and the index of where it
    /// was found; <see langword="null"/> if no match was found.
    /// </returns>
    public virtual Token? Match(string input, IEnumerable<Rule> rules)
        => _match(input, rules);

    public override bool Equals(object? obj)
        => obj is not null && obj is Rule r && r._match == _match && r.Parse == Parse;

    public override int GetHashCode()
        => HashCode.Combine(Parse, Match);

    public static bool operator ==(Rule a, Rule b)
        => a.Equals(b);

    public static bool operator !=(Rule a, Rule b)
        => !a.Equals(b);
}
