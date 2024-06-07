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
                return (Number)Convert.ToDouble(value.Value);
        }
        catch
        {
        }

        return Constant.UNDEFINED;
    }

    public T? ConvertFromExpression(IExpression expression)
    {
        try
        {
            if (expression is Number num)
            {
                if (num.Value > Convert.ToDouble(T.MaxValue))
                    return T.PositiveInfinity;
                else if (num.Value < Convert.ToDouble(T.MinValue))
                    return T.NegativeInfinity;
                
                return (T?)Convert.ChangeType(num.Value, typeof(T));
            }
        }
        catch
        {
        }

        return null;
    }
}
