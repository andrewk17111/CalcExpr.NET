using CalcExpr.Expressions;
using CalcExpr.Expressions.Collections;

namespace TestCalcExpr.TestUtils;

internal static class UtilFunctions
{
    public static T Range<T>(int start, int count)
        where T : IEnumerableExpression
        => (T)T.ConvertIEnumerable(Enumerable.Range(start, count).Select(i => (Number)i));

    public static T Random<T>(int count, int min = Int32.MinValue, int max = Int32.MaxValue)
        where T : IEnumerableExpression
    {
        Random random = new Random();

        return (T)T.ConvertIEnumerable(
            new int[count].Select(i => (Number)(random.Next(min, max) + random.NextDouble())));
    }

    public static IEnumerable<IExpression> Random(int count, int min = Int32.MinValue, int max = Int32.MaxValue)
    {
        Random random = new Random();

        return new int[count].Select(i => (Number)(random.Next(min, max) + random.NextDouble()));
    }
}
