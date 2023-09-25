using CalcExpr.Expressions;
using CalcExpr.Parsing;
using CalcExpr.Parsing.Rules;
using System.Text.RegularExpressions;

namespace TestCalcExpr;

[TestClass]
public class TestParser
{
    readonly RegexRule CUSTOM_RULE = new RegexRule("Char", @"[A-Z]", RegexOptions.None,
        (expression, token, _) => new Number(token.Value[0] - 65));
    
    /// <summary>
    /// Tests that the Parser can be initialized properly from either constructor.
    /// </summary>
    [TestMethod]
    public void TestInit()
    {
        const string OPERAND = @"({Prefix}*({Variable}|{Constant}|{Number}|\[\d+\]){Postfix}*)";

        (string, string)[] default_rules =
        {
            ("AssignBinOp", @$"(?<={OPERAND})(?<!!)(=)(?={OPERAND})"),
            ("OrBinOp", @$"(?<={OPERAND})(\|\||∨)(?={OPERAND})"),
            ("XorBinOp", @$"(?<={OPERAND})(⊕)(?={OPERAND})"),
            ("AndBinOp", @$"(?<={OPERAND})(&&|∧)(?={OPERAND})"),
            ("EqBinOp", @$"(?<={OPERAND})(==|!=|<>|≠)(?={OPERAND})"),
            ("IneqBinOp", @$"(?<={OPERAND})(>=|<=|<(?!>)|(?<!<)>|[≤≥])(?={OPERAND})"),
            ("AddBinOp", @$"(?<={OPERAND})([\+\-])(?={OPERAND})"),
            ("MultBinOp", @$"(?<={OPERAND})(%%|//|[*×/÷%])(?={OPERAND})"),
            ("ExpBinOp", @$"(?<={OPERAND})(\^)(?={OPERAND})"),
            ("Prefix", @"[\+\-!~¬]"),
            ("Postfix", @"(((?<![A-Za-zΑ-Ωα-ω0-9](!!)*!)!!)|[!%#])"),
            ("Constant", "(∞|(inf(inity)?)|π|pi|τ|tau|e|true|false)"),
            ("Variable", "([A-Za-zΑ-Ωα-ω]+(_[A-Za-zΑ-Ωα-ω0-9]+)*)"),
            ("Number", @"((\d+\.?\d*)|(\d*\.?\d+))"),
        };

        Parser parser = new Parser();

        for (int i = 0; i < default_rules.Length; i++)
        {
            RegexRule rr = (RegexRule)parser.Grammar[i];

            Assert.AreEqual(rr.Name, default_rules[i].Item1);
            Assert.AreEqual(rr.RegularExpression, default_rules[i].Item2);
        }

        parser = new Parser(new Rule[] { CUSTOM_RULE });

        Assert.IsTrue(parser.Grammar.Length == 1);
        Assert.IsTrue(parser.Grammar[0] == CUSTOM_RULE);
    }

    /// <summary>
    /// Tests that the Parser is able to properly parse strings into IExpressions.
    /// </summary>
    [TestMethod]
    public void TestParse()
    {
        foreach ((string expression_string, IExpression expression, _) in TestCases.Expressions)
        {
            Console.WriteLine(expression_string);
            Assert.AreEqual(expression, new Parser().Parse(expression_string));
        }
    }

    /// <summary>
    /// Tests that previously parsed expressions get added to the Parser's cache, expressions can be manually added and
    /// removed from the cache, a string can be found in the cache, and the cache can be cleared.
    /// </summary>
    [TestMethod]
    public void TestCache()
    {
        Parser parser = new Parser();

        foreach ((string expression, _, _) in TestCases.Expressions)
        {
            parser.Parse(expression);

            bool cached = parser.ContainsCache(expression);

            if (Regex.IsMatch(expression, @"\(|\)"))
            {
                Assert.IsFalse(cached);
            }
            else
            {
                Assert.IsTrue(cached);
                parser.RemoveCache(expression);
                Assert.IsFalse(parser.ContainsCache(expression));
            }
        }

        (string, IExpression) pi = ("pi", new Number(3.1415926535));
        
        parser.AddCache(pi.Item1, pi.Item2);
        Assert.IsTrue(parser.ContainsCache(pi.Item1));
        parser.RemoveCache(pi.Item1);
        Assert.IsFalse(parser.ContainsCache(pi.Item1));
    }

    /// <summary>
    /// Tests that Rules can be added and removed from the Parser's grammar along with being able to change the priority
    /// of Rules.
    /// </summary>
    [TestMethod]
    public void TestGrammar()
    {
        Parser parser = new Parser();
        RegexRule tau = new RegexRule("tau", "tau", RegexOptions.None, (expression, token, _) => new Number(6.28));
        
        Assert.IsFalse(parser.Grammar.Contains(CUSTOM_RULE));
        Assert.IsFalse(parser.Grammar.Contains(tau));
        Assert.IsTrue(parser.AddGrammarRule(CUSTOM_RULE, 0));
        Assert.IsTrue(parser.Grammar[0] == CUSTOM_RULE);
        Assert.IsTrue(parser.AddGrammarRule(tau, -1));
        Assert.IsTrue(parser.Grammar.Last() == tau);
        Assert.IsTrue(parser.GrammarContains(tau.Name));
        Assert.IsTrue(parser.RemoveGrammarRule(CUSTOM_RULE.Name));
        Assert.IsFalse(parser.GrammarContains(CUSTOM_RULE.Name));
        Assert.IsTrue(parser.RemoveGrammarRuleAt(parser.Grammar.Length - 1));
        Assert.IsFalse(parser.GrammarContains(tau.Name));
    }
}
