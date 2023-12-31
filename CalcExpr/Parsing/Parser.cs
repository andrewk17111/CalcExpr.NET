﻿using CalcExpr.Exceptions;
using CalcExpr.Expressions;
using System.Text.RegularExpressions;

namespace CalcExpr.Parsing;

public class Parser
{
    private readonly List<Rule> _grammar = new List<Rule>();
    private readonly Dictionary<string, IExpression> _cache = new Dictionary<string, IExpression>();

    public Rule[] Grammar
        => _grammar.ToArray();

    public string[] Cache
        => _cache.Keys.ToArray();

    /// <summary>
    /// Creates a <see cref="Parser"/> with the default grammar.
    /// </summary>
    public Parser()
    {
        const string OPERAND = @"([\+\-!~¬]*((\d+\.?\d*)|(\d*\.?\d+)|\[\d+\])[%!]*)";

        _grammar = new List<Rule>()
        {
            new Rule(@$"(?<={OPERAND})(\|\||∨)(?={OPERAND})", ParseBinaryOperator),
            new Rule(@$"(?<={OPERAND})(⊕)(?={OPERAND})", ParseBinaryOperator),
            new Rule(@$"(?<={OPERAND})(&&|∧)(?={OPERAND})", ParseBinaryOperator),
            new Rule(@$"(?<={OPERAND})(==|!=|<>|≠)(?={OPERAND})", ParseBinaryOperator),
            new Rule(@$"(?<={OPERAND})(>=|<=|<(?!>)|(?<!<)>|[≤≥])(?={OPERAND})", ParseBinaryOperator),
            new Rule(@$"(?<={OPERAND})([\+\-])(?={OPERAND})", ParseBinaryOperator),
            new Rule(@$"(?<={OPERAND})(%%|//|[*×/÷%])(?={OPERAND})", ParseBinaryOperator),
            new Rule(@$"(?<={OPERAND})(\^)(?={OPERAND})", ParseBinaryOperator),
            new Rule(@"(?<=^\s*)[\+\-!~¬]", ParsePrefix),
            new Rule(@"[%!](?=\s*$)", ParsePostfix),
            new Rule(@"(?<=^\s*)((\d+\.?\d*)|(\d*\.?\d+))(?=\s*$)", ParseNumber)
        };
    }

    /// <summary>
    /// Create a <see cref="Parser"/> using the specified grammar.
    /// </summary>
    /// <param name="grammar">
    /// The specified <see cref="IEnumerable{Rule}"/> to be used as the grammar of the <see cref="Parser"/>.
    /// </param>
    public Parser(IEnumerable<Rule> grammar)
        => _grammar = grammar.ToList();

    /// <summary>
    /// Parses an expression <see cref="string"/> into an <see cref="IExpression"/>.
    /// </summary>
    /// <param name="input">The expression <see cref="string"/> to parse.</param>
    /// <returns>An <see cref="IExpression"/> parsed from the specified expression <see cref="string"/>.</returns>
    /// <exception cref="ArgumentNullException"></exception>
    /// <exception cref="Exception"></exception>
    public IExpression Parse(string input)
    {
        if (input is null)
            throw new ArgumentNullException(nameof(input));

        string clean_input = CleanExpressionString(input);

        if (Regex.IsMatch(input, @"\(|\)"))
            return ParseWithParentheses(clean_input);

        if (ContainsCache(clean_input))
            return _cache[clean_input].Clone();

        foreach (Rule rule in _grammar)
        {
            Match match = Regex.Match(clean_input, rule.RegularExpression, RegexOptions.RightToLeft);

            if (match.Success)
            {
                IExpression expression = rule.Parse.Invoke(clean_input, match);

                AddCache(clean_input, expression);
                return expression;
            }
        }

        throw new Exception($"The input was not in the correct format: '{input}'");
    }

    private static string CleanExpressionString(string expression)
        => Regex.Replace(expression, @"\s+", "");

    private IExpression ParseWithParentheses(string input)
    {
        string tokenized_input = TokenizeInput(input, out Token[] tokens);

        if (tokenized_input == "[0]")
            return new Parentheses(Parse(input[1..^1]));

        foreach (Rule rule in _grammar)
        {
            Match match = Regex.Match(tokenized_input, rule.RegularExpression, RegexOptions.RightToLeft);

            if (match.Success)
                return rule.Parse.Invoke(input,
                    new Token(match.Value, DetokenizeIndex(match.Index, tokenized_input, tokens)));
        }

        throw new Exception($"The input was not in the correct format: '{input}'");
    }

    private static string TokenizeInput(string input, out Token[] tokens)
    {
        input = Regex.Replace(input, @"[\[\]\\]", match => @$"\{match.Value}");

        List<Token> toks = new List<Token>();
        string output = String.Empty;
        int start = -1;
        int depth = 0;

        for (int i = 0; i < input.Length; i++)
        {
            char current = input[i];

            if (current == '(')
            {
                if (start == -1)
                {
                    start = i;
                    depth = 1;
                }
                else
                {
                    depth++;
                }
            }
            else if (current == ')')
            {
                if (start < 0)
                {
                    throw new UnbalancedParenthesesException(input);
                }
                else if (depth > 1)
                {
                    depth--;
                }
                else
                {
                    output += $"[{toks.Count}]";
                    toks.Add(new Token(input[start..(i + 1)], start));
                    start = -1;
                    depth = 0;
                }
            }
            else
            {
                if (start < 0)
                    output += current;
                else if (i == input.Length - 1)
                    throw new UnbalancedParenthesesException(input);
            }
        }

        tokens = toks.ToArray();
        return output;
    }

    private static int DetokenizeIndex(int index, string tokenized_string, Token[] tokens)
    {
        Match[] matches = Regex.Matches(tokenized_string[..index], @"((?<=^|([^\\]([\\][\\])*))\[\d+\])|(\\[\[\]\\])")
            .ToArray();

        foreach (Match match in matches)
            if (match.Value.StartsWith("\\"))
                index -= 1;
            else
                index += tokens[Convert.ToInt32(match.Value[1..^1])].Length - 3;

        return index;
    }

    /// <summary>
    /// Determines whether the cache of the <see cref="Parser"/> contains a specified expression <see cref="string"/>.
    /// </summary>
    /// <param name="expression">The expression to locate in the cache.</param>
    /// <returns>
    /// <see langword="true"/> if the cache contains an entry with the specified expression; otherwise, 
    /// <see langword="false"/>.
    /// </returns>
    public bool ContainsCache(string expression)
        => _cache.ContainsKey(CleanExpressionString(expression));

    /// <summary>
    /// Add expression <see cref="string"/> to the cache of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="key">The expression <see cref="string"/> of the cached <see cref="IExpression"/>.</param>
    /// <param name="value">The cached <see cref="IExpression"/>.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="IExpression"/> was successfully added to the cache; otherwise, 
    /// <see langword="false"/>.
    /// </returns>
    public bool AddCache(string key, IExpression value)
    {
        try
        {
            _cache[CleanExpressionString(key)] = value.Clone();
            return true;
        }
        catch
        {
            return false;
        }
    }

    /// <summary>
    /// Removes a cached <see cref="IExpression"/> based on the specified expression <see cref="string"/>.
    /// </summary>
    /// <param name="expression">The expression <see cref="string"/> of the cached <see cref="IExpression"/>.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="IExpression"/> was successfully removed from the cache; otherwise,
    /// <see langword="false"/>.
    /// </returns>
    public bool RemoveCache(string expression)
    {
        string clean_expression = CleanExpressionString(expression);

        return _cache.ContainsKey(clean_expression) && _cache.Remove(clean_expression);
    }

    /// <summary>
    /// Add <see cref="Rule"/> to the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="rule">The <see cref="Rule"/> to be added to the grammar.</param>
    /// <param name="index">The index to put the <see cref="Rule"/> in the grammar.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="Rule"/> was successfully added to the grammar; otherwise, 
    /// <see langword="false"/>.
    /// </returns>
    public bool AddGrammarRule(Rule rule, int index = -1)
    {
        if (index < 0)
            index += _grammar.Count + 1;

        try
        {
            if (index <= 0)
                _grammar.Insert(0, rule);
            else if (index >= _grammar.Count)
                _grammar.Insert(_grammar.Count, rule);
            else
                _grammar.Insert(index, rule);

            return true;
        }
        catch
        {
            return false;
        }
    }

    /// <summary>
    /// Add <see cref="Rule"/> to the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="regex">The regular expression to match for the new <see cref="Rule"/>.</param>
    /// <param name="parse_func">
    /// The <see cref="Func{string, Token, IExpression}"/> of the new <see cref="Rule"/> to parse the matched 
    /// <see cref="Token"/>.
    /// </param>
    /// <param name="index">The index to put the <see cref="Rule"/> in the grammar.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="Rule"/> was successfully added to the grammar; otherwise, 
    /// <see langword="false"/>.
    /// </returns>
    public bool AddGrammarRule(string regex, Func<string, Token, IExpression> parse_func, int index = -1)
        => AddGrammarRule(new Rule(regex, parse_func), index);

    /// <summary>
    /// Removes the <see cref="Rule"/> with the specified regex <see cref="string"/> from the grammar of the 
    /// <see cref="Parser"/>.
    /// </summary>
    /// <param name="regex">The regex <see cref="string"/> for the <see cref="Rule"/> to be removed.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="Rule"/> was successfully removed; otherwise, <see langword="false"/>.
    /// </returns>
    public bool RemoveGrammarRule(string regex)
    {
        for (int i = 0; i < _grammar.Count; i++)
            if (_grammar[i].RegularExpression == regex)
                return RemoveGrammarRuleAt(i);

        return false;
    }

    /// <summary>
    /// Removes the <see cref="Rule"/> at the specified index from the grammar of the <see cref="Parser"/>.
    /// </summary>
    /// <param name="index">The index for the <see cref="Rule"/> to be removed.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="Rule"/> was successfully removed; otherwise, <see langword="false"/>.
    /// </returns>
    public bool RemoveGrammarRuleAt(int index)
    {
        try
        {
            _grammar.RemoveAt(index);
            return true;
        }
        catch
        {
            return false;
        }
    }

    /// <summary>
    /// Determines whether a rule with the specified regex <see cref="string"/> is in the grammar of the 
    /// <see cref="Parser"/>.
    /// </summary>
    /// <param name="regex">The regex <see cref="string"/> of the <see cref="Rule"/> to find.</param>
    /// <returns>
    /// <see langword="true"/> if the <see cref="Rule"/> was successfully found; otherwise, <see langword="false"/>.
    /// </returns>
    public bool GrammarContains(string regex)
    {
        foreach (Rule rule in _grammar)
            if (rule.RegularExpression == regex)
                return true;

        return false;
    }

    private IExpression ParseBinaryOperator(string input, Token match)
        => new BinaryOperator(match.Value, Parse(input[..match.Index]), Parse(input[(match.Index + match.Length)..]));

    private IExpression ParsePrefix(string input, Token match)
        => new UnaryOperator(match.Value, true, Parse(input[match.Length..]));

    private IExpression ParsePostfix(string input, Token match)
        => new UnaryOperator(match.Value, false, Parse(input[..^match.Length]));
    
    private static IExpression ParseNumber(string input, Token match)
        => new Number(Convert.ToDouble(match.Value));
}
