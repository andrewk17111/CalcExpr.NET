using CalcExpr.Expressions;
using CalcExpr.Expressions.Collections;

namespace TestCalcExpr.TestUtils;

internal static class UtilFunctions
{
    public static T Range<T>(int start, int count)
        where T : IEnumerableExpression
        => (T)T.ConvertIEnumerable(Enumerable.Range(start, count).Select(i => (Number)i));

    public static T Range<T>(int start, int count, int step)
        where T : IEnumerableExpression
        => (T)T.ConvertIEnumerable(Enumerable.Range(0, count).Select(i => (Number)(start + i * step)));

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

    public static void AreEqual(IExpression expected, IExpression actual, int decimal_places = 0, string message = "")
    {
        if (expected is IEnumerable<IExpression> exp_enum && actual is IEnumerable<IExpression> act_enum)
        {
            if (exp_enum.GetType() == act_enum.GetType() && exp_enum.Count() == act_enum.Count())
            {
                IEnumerator<IExpression> exp_enumerator = exp_enum.GetEnumerator();
                IEnumerator<IExpression> act_enumerator = act_enum.GetEnumerator();

                while (exp_enumerator.MoveNext() && act_enumerator.MoveNext())
                    AreEqual(exp_enumerator.Current, act_enumerator.Current, decimal_places, message);

                return;
            }
        }
        else if (expected is Number exp_num && actual is Number act_num)
        {
            Assert.AreEqual(Math.Round(exp_num.Value, decimal_places), Math.Round(act_num.Value, decimal_places),
                message);
            return;
        }
        
        Assert.AreEqual(expected, actual, message);
    }

    public static void IsLessThan(double expected, double actual, string message = "")
    {
        if (actual >= expected)
        {
            string text = $"Expected: {expected}. Actual: {actual}.";

            if (!String.IsNullOrEmpty(message))
                text += $" {message}";

            HandleFail("UtilFunctions.IsLessThan", text);
        }
    }
    
    public static void IsLessThanOrEqual(double expected, double actual, string message = "")
    {
        if (actual > expected)
        {
            string text = $"Expected: {expected}. Actual: {actual}.";

            if (!String.IsNullOrEmpty(message))
                text += $" {message}";

            HandleFail("UtilFunctions.IsLessThanOrEqual", text);
        }
    }
    
    public static void IsGreaterThan(double expected, double actual, string message = "")
    {
        if (actual <= expected)
        {
            string text = $"Expected: {expected}. Actual: {actual}.";

            if (!String.IsNullOrEmpty(message))
                text += $" {message}";

            HandleFail("UtilFunctions.IsGreaterThan", text);
        }
    }
    
    public static void IsGreaterThanOrEqual(double expected, double actual, string message = "")
    {
        if (actual < expected)
        {
            string text = $"Expected: {expected}. Actual: {actual}.";

            if (!String.IsNullOrEmpty(message))
                text += $" {message}";

            HandleFail("UtilFunctions.IsGreaterThanOrEqual", text);
        }
    }

    private static void HandleFail(string assertion_name, string message)
    {
        string text = $"{assertion_name} failed.";

        if (!String.IsNullOrEmpty(message))
            text += $" {message}";

        throw new AssertFailedException(text);
    }
}
