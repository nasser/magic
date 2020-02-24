using System;
using System.Linq;
using System.Globalization;
using System.Reflection;

namespace Magic
{
    public class Binder : System.Reflection.Binder
    {
        public static readonly Binder Shared = new Binder();

        System.Reflection.Binder _binder = Type.DefaultBinder;

        public override FieldInfo BindToField(BindingFlags bindingAttr, FieldInfo[] match, object value, CultureInfo culture)
        {
            return _binder.BindToField(bindingAttr, match, value, culture);
        }

        public override MethodBase? BindToMethod(BindingFlags bindingAttr, MethodBase[] match, ref object[] args, ParameterModifier[]? modifiers, CultureInfo? culture, string[]? names, out object state)
        {
            try {
                var nativeResult = _binder.BindToMethod(bindingAttr, match, ref args, modifiers, culture, names, out state);
                if(nativeResult != null)
                    return nativeResult;
            } catch(MissingMemberException) {
                // ignore
            }
            var argumentTypes = args.Select(a => a.GetType()).ToArray();
            state = new Object(); // ???
            var result = SelectMethod(bindingAttr, match, argumentTypes, modifiers);
            if(result != null)
            {
                var parameters = result.GetParameters();
                for (int i = 0; i < args.Length; i++)
                {
                    // this is assumed to work at this point. a situation where
                    // we could not convert the argument types should not have
                    // bound and SelectMethod should have returned null
                    args[i] = Convert.ChangeType(args[i], parameters[i].ParameterType);
                }
            }

            return result;
        }

        public override object ChangeType(object value, Type type, CultureInfo culture)
        {
            return _binder.ChangeType(value, type, culture);
        }

        public override void ReorderArgumentArray(ref object[] args, object state)
        {
            _binder.ReorderArgumentArray(ref args, state);
        }
        
        public override MethodBase? SelectMethod(BindingFlags bindingAttr, MethodBase[] match, Type[] argumentTypes, ParameterModifier[]? modifiers)
        {
            if(match.Length == 0)
                return null;
            MethodBase result;
            result = _binder.SelectMethod(bindingAttr, match, argumentTypes, modifiers);
            if (result != null)
                return result;
            foreach (var candidate in match)
            {
                if (MatchByNarrowingConversion(bindingAttr, candidate, argumentTypes, modifiers))
                {
                    if (result != null)
                        throw new AmbiguousMatchException();
                    else
                        result = candidate;
                }
            }

            return result;
        }

        static bool MatchByNarrowingConversion(BindingFlags bindingAttr, MethodBase candidate, Type[] argumentTypes, ParameterModifier[]? modifiers)
        {
            var parameters = candidate.GetParameters();
            if (parameters.Length != argumentTypes.Length) return false;
            for (int i = 0; i < parameters.Length; i++)
            {
                var parameterType = parameters[i].ParameterType;
                var argumentType = argumentTypes[i];
                if(parameterType == argumentType
                   || (parameterType.IsPrimitive && argumentType.IsPrimitive && parameterType != typeof(Boolean) && argumentType != typeof(Boolean))
                   || parameterType.IsAssignableFrom(argumentType))
                {
                    continue;
                }
                else 
                {
                    return false;
                }
            }
            return true;
        }

        public override PropertyInfo SelectProperty(BindingFlags bindingAttr, PropertyInfo[] match, Type returnType, Type[] indexes, ParameterModifier[] modifiers)
        {
            return _binder.SelectProperty(bindingAttr, match, returnType, indexes, modifiers);
        }
    }
}