using CalcExpr.Expressions;
using CalcExpr.Parsing;

namespace TestCalcExpr;

[TestClass]
public class TestParser
{
    readonly Rule CUSTOM_RULE = new Rule(@"[A-Z", (expression, token) => new Number(token.Value[0] - 65));
    readonly Dictionary<string, IExpression> EXPRESSIONS = new Dictionary<string, IExpression>()
    {
        { ".1", new Number(0.1) },
        { "0.1", new Number(0.1) },
        { "1.", new Number(1) },
        { "1", new Number(1) },
        { "+1", new UnaryOperator("+", true, new Number(1)) },
        { "-1", new UnaryOperator("-", true, new Number(1)) },
        { "!1", new UnaryOperator("!", true, new Number(1)) },
        { "~2", new UnaryOperator("~", true, new Number(2)) },
        { "¬0", new UnaryOperator("¬", true, new Number(0)) },
        { "5!", new UnaryOperator("!", false, new Number(5)) },
        { "1%", new UnaryOperator("%", false, new Number(1)) },
        { "~!1", new UnaryOperator("~", true, new UnaryOperator("!", true, new Number(1))) },
        { "2!%", new UnaryOperator("%", false, new UnaryOperator("!", false, new Number(2))) },
        { "-5%", new UnaryOperator("-", true, new UnaryOperator("%", false, new Number(5))) }
    };

    /// <summary>
    /// Tests that the Parser can be initialized properly from either constructor.
    /// </summary>
    [TestMethod]
    public void TestInit()
    {
        string[] default_rules =
        {
            @"(?<=^\s*)[\+\-!~¬]",
            @"[%!](?=\s*$)",
            @"(?<=^\s*)((\d+\.?\d*)|(\d*\.?\d+))(?=\s*$)",
        };

        Parser parser = new Parser();

        for (int i = 0; i < default_rules.Length; i++)
            Assert.IsTrue(parser.Grammar[i].RegularExpression == default_rules[i]);

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
        foreach (string expression in EXPRESSIONS.Keys)
            Assert.AreEqual(EXPRESSIONS[expression], new Parser().Parse(expression));
    }

    /// <summary>
    /// Tests that previously parsed expressions get added to the Parser's cache, expressions can be manually added and
    /// removed from the cache, a string can be found in the cache, and the cache can be cleared.
    /// </summary>
    [TestMethod]
    public void TestCache()
    {
        Parser parser = new Parser();

        foreach (string expression in EXPRESSIONS.Keys)
        {
            parser.Parse(expression);
            Assert.IsTrue(parser.ContainsCache(expression));
            parser.RemoveCache(expression);
            Assert.IsFalse(parser.ContainsCache(expression));
        }

        (string, IExpression) pi = ("pi", new Number(3.1415926535));
        
        parser.AddCache(pi.Item1, pi.Item2);
        Assert.IsTrue(parser.ContainsCache(pi.Item1));
        parser.RemoveCacheAt(0);
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
        Rule tau = new Rule("tau", (expression, token) => new Number(6.28));
        
        Assert.IsFalse(parser.Grammar.Contains(CUSTOM_RULE));
        Assert.IsFalse(parser.Grammar.Contains(tau));
        Assert.IsTrue(parser.AddGrammarRule(CUSTOM_RULE, -1));
        Assert.IsTrue(parser.Grammar[0] == CUSTOM_RULE);
        Assert.IsTrue(parser.AddGrammarRule(tau.RegularExpression, tau.Parse, -1));
        Assert.IsTrue(parser.Grammar.Last() == tau);
        Assert.IsTrue(parser.GrammarContains(tau.RegularExpression));
        Assert.IsTrue(parser.RemoveGrammarRule(CUSTOM_RULE.RegularExpression));
        Assert.IsFalse(parser.GrammarContains(CUSTOM_RULE.RegularExpression));
        Assert.IsTrue(parser.RemoveGrammarRuleAt(0));
        Assert.IsFalse(parser.GrammarContains(tau.RegularExpression));
    }
}
