using CalcExpr.Tokenization;
using CalcExpr.Tokenization.Tokens;
using TestCalcExpr.TestData;
using TestCalcExpr.TestUtils;

namespace TestCalcExpr.Tokenization;

[TestClass]
public class TestTokenizer
{
    [TestMethod]
    public void TestEmptyTokenizer()
    {
        string input = "Hello World!";
        Tokenizer tokenizer = new Tokenizer([], false);
        List<IToken> tokens = tokenizer.Tokenize(input);

        Assert.AreEqual(input, string.Join("", tokens.Select(t => t.Value)));
    }

    /// <summary>
    /// Tests that the Tokenizer is able to properly tokenize strings.
    /// </summary>
    [TestMethod]
    public void TestTokenize()
    {
        TokenizeTestCases(TestCases.Expressions);
    }

    /// <summary>
    /// Tests that the Tokenizer is able to properly tokenize strings for collections.
    /// </summary>
    [TestMethod]
    public void TestTokenizeCollections()
    {
        TokenizeTestCases(TestCases.Collections);
    }

    private static void TokenizeTestCases(IEnumerable<TestCase> testCases)
    {
        Tokenizer tokenizer = new Tokenizer();

        foreach (TestCase testCase in testCases)
        {
            List<IToken> tokenized = tokenizer.Tokenize(testCase.ExpressionString);

            Assert.AreEqual(testCase.Tokenized, tokenized);
        }
    }
}
