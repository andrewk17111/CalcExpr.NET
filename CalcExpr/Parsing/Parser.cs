using CalcExpr.Expressions;
using System.Text.RegularExpressions;

namespace CalcExpr.Parsing;

public class Parser
{
    private readonly List<Rule> _grammar = new List<Rule>();

    public Parser()
    {
        _grammar = new List<Rule>()
        {
            new Rule(@"(?<=^\s*)((\d+\.?\d*)|(\d*\.?\d+))(?=\s*$)", ParseNumber)
        };
    }

    public Parser(List<Rule> grammar)
        => _grammar = grammar;

    public IExpression Parse(string input)
    {
        if (input is null)
            throw new ArgumentNullException(nameof(input));

        string clean_input = Regex.Replace(input, @"\s+", " ");

        foreach (Rule rule in _grammar)
        {
            Match match = Regex.Match(input, rule.RegularExpression);

            if (match.Success)
                return rule.Parse.Invoke(input, match);
        }

        throw new Exception($"The input was not in the correct format: '{input}'");
    }

    private static IExpression ParseNumber(string input, Token match)
        => new Number(Convert.ToDouble(match.Value));
}
