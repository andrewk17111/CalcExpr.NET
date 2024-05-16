using CalcExpr.Expressions;
using CalcExpr.Expressions.Collections;
using TestCalcExpr.TestUtils;

namespace TestCalcExpr.Expressions.Collections;

[TestClass]
public class TestIndexer
{
    /// <summary>
    /// Tests that the values of the Indexer are the same as the value passed into the constructor.
    /// </summary>
    [TestMethod]
    public void TestInit()
    {
        for (int i = 0; i < 20; i++)
        {
            Vector elements = new Vector(UtilFunctions.Random(i));

            for (int index = 0; index < elements.Length; index++)
            {
                Indexer indexer = new Indexer(elements, new Number(index));

                Assert.AreEqual(elements, indexer.Collection);
                Assert.AreEqual(index, indexer.Index);
            }
        }
    }

    /// <summary>
    /// Tests that the Indexer converts to a string properly.
    /// </summary>
    [TestMethod]
    public void TestToString()
    {
        for (int i = 0; i < 20; i++)
        {
            IExpression[] elements = [.. UtilFunctions.Random(i)];
            Vector vect = new Vector(elements);
            Indexer indexer = new Indexer(vect, new Number(i));

            Assert.AreEqual($"{vect}[{i}]", indexer.ToString());
        }
    }
}
