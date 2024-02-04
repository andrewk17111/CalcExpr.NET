using CalcExpr.Exceptions;
using CalcExpr.Expressions;
using System.Text.RegularExpressions;

namespace CalcExpr.Parsing.Rules;

/// <summary>
/// A rule to be used to parse a <see cref="string"/> into an <see cref="IExpression"/>.
/// </summary>
/// <param name="name">The name of the <see cref="NestedRegexRule"/>.</param>
/// <param name="regex">The regex <see cref="string"/> to match an expression <see cref="string"/> to.</param>
/// <param name="options">
/// The options for the regular expression along with additional options finding a match.
/// </param>
/// <param name="parse">
/// The function to use to parse a <see cref="string"/> into an <see cref="IExpression"/>.
/// </param>
public class NestedRegexRule(string name, string regex, RegexRuleOptions options,
    Func<string, Token, Parser, IExpression> parse) : RegexRule(name, regex, options, parse)
{
    /// <summary>
    /// A rule to be used to parse a <see cref="string"/> into an <see cref="IExpression"/>.
    /// </summary>
    /// <param name="name">The name of the <see cref="NestedRegexRule"/>.</param>
    /// <param name="regex">The regex <see cref="string"/> to match an expression <see cref="string"/> to.</param>
    /// <param name="options">The options for the regular expression.</param>
    /// <param name="parse">
    /// The function to use to parse a <see cref="string"/> into an <see cref="IExpression"/>.
    /// </param>
    public NestedRegexRule(string name, string regex, RegexOptions options,
        Func<string, Token, Parser, IExpression> parse)
        : this(name, regex, (RegexRuleOptions)options, parse)
    { }

    public override Token? Match(string input, IEnumerable<Rule> rules)
    {
        Dictionary<string, string> preprocessed_rules = [];
        string regex = ReplaceRules(RegularExpression, rules, Options, ref preprocessed_rules);

        return FindMatch(input, regex, Options);
    }

    private static string ReplaceRules(string regular_expression, IEnumerable<Rule> rules, RegexRuleOptions options,
        ref Dictionary<string, string> preprocessed_rules)
    {
        List<string> matches = Regex.Matches(regular_expression, @"(?<=(^|[^\\](\\\\)*){)\w+(?=})")
            .Select(m => m.Value)
            .Distinct()
            .ToList();

        if (matches.Count > 0)
        {
            string regex = regular_expression;
            Dictionary<string, int> rule_names = rules.Select((r, i) => new KeyValuePair<string, int>(r.Name, i))
                .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

            while (matches.Count > 0)
            {
                string match = matches.First();
                string sub_regex;

                if (preprocessed_rules.TryGetValue(match, out string? value))
                {
                    sub_regex = value;
                    matches.Remove(match);
                }
                else if (rule_names.TryGetValue(match, out int idx))
                {
                    if (rules.ElementAt(idx) is NestedRegexRule nrr)
                    {
                        sub_regex = ReplaceRules(nrr.RegularExpression, rules, options, ref preprocessed_rules);
                        preprocessed_rules.Add(match, sub_regex);
                    }
                    else if (rules.ElementAt(idx) is RegexRule rr)
                    {
                        sub_regex = rr.RegularExpression;
                    }
                    else
                    {
                        throw new NonRegexRuleException(match, rules.ElementAt(idx).GetType());
                    }

                    rule_names.Remove(match);
                    matches.Remove(match);
                }
                else
                {
                    continue;
                }

                if (options.HasFlag(RegexRuleOptions.PadReferences))
                    sub_regex = $@"(\s*{sub_regex}\s*)";

                regex = Regex.Replace(regex, @$"(?<=^|[^\\](\\\\)*){{{match}}}", sub_regex);
            }

            return regex;
        }

        return regular_expression;
    }

    public override bool Equals(object? obj)
        => obj is not null && obj is RegexRule a && RegularExpression == a.RegularExpression;

    public override int GetHashCode()
        => RegularExpression.GetHashCode();

    public static bool operator ==(NestedRegexRule a, NestedRegexRule b)
        => a.Equals(b);

    public static bool operator !=(NestedRegexRule a, NestedRegexRule b)
        => !a.Equals(b);
}
