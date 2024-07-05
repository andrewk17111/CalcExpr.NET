using CalcExpr.Expressions;
using System.Numerics;

namespace CalcExpr.TypeConverters;

public class FloatTypeConverter<T> : ITypeConverter<T?>
    where T : struct, IFloatingPointIeee754<T>, IMinMaxValue<T>
{
    public IExpression ConvertToExpression(T? value)
    {
        try
        {
            if (value.HasValue)
            {
                if (T.IsPositiveInfinity(value.Value))
                    return Constant.INFINITY;
                else if (T.IsNegativeInfinity(value.Value))
                    return Constant.NEGATIVE_INFINITY;
                else if (T.IsNaN(value.Value))
                    return Constant.UNDEFINED;

                return (Number)Convert.ToDouble(value.Value);
            }
        }
        catch
        {
        }

        return Constant.UNDEFINED;
    }

    public T? ConvertFromExpression(IExpression? expression)
    {
        try
        {
            if (expression is not null && expression is Number num)
            {
                if (num.Value > Convert.ToDouble(T.MaxValue))
                    return T.PositiveInfinity;
                else if (num.Value < Convert.ToDouble(T.MinValue))
                    return T.NegativeInfinity;
                
                return (T?)Convert.ChangeType(num.Value, typeof(T));
            }
            else if (expression is Constant)
            {
                if (Constant.INFINITY.Equals(expression))
                    return T.PositiveInfinity;
                else if (Constant.NEGATIVE_INFINITY.Equals(expression))
                    return T.NegativeInfinity;
                else if (Constant.TRUE.Equals(expression))
                    return T.One;
                else if (Constant.FALSE.Equals(expression))
                    return T.Zero;
            }
        }
        catch
        {
        }

        return null;
    }
}
