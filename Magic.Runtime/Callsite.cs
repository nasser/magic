using System;
using System.Reflection;
using System.Linq;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace Magic
{
    // delegates that takes values and return values
    // used unless method return is void (use CallsiteAction* instead) or method
    // is an instance method of a valuetype (use Callsite*Ref instead).
    // Func exists in newer .NETs but these are backported for .NET 3.5 support
    public delegate TResult CallsiteFunc<in T1,out TResult>(T1 arg1);
    public delegate TResult CallsiteFunc<in T1,in T2,out TResult>(T1 arg1, T2 arg2);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,out TResult>(T1 arg1, T2 arg2, T3 arg3);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17,in T18,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17, T18 arg18);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17,in T18,in T19,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17, T18 arg18, T19 arg19);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17,in T18,in T19,in T20,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17, T18 arg18, T19 arg19, T20 arg20);
    
    // delegates that return void
    // used for methods that return void (except instance methods of valuetypes, which need CallsiteActionRef)
    public delegate void CallsiteAction<in T1>(T1 arg1);
    public delegate void CallsiteAction<in T1,in T2>(T1 arg1, T2 arg2);
    public delegate void CallsiteAction<in T1,in T2,in T3>(T1 arg1, T2 arg2, T3 arg3);
    public delegate void CallsiteAction<in T1,in T2,in T3,in T4>(T1 arg1, T2 arg2, T3 arg3, T4 arg4);
    public delegate void CallsiteAction<in T1,in T2,in T3,in T4,in T5>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5);
    public delegate void CallsiteAction<in T1,in T2,in T3,in T4,in T5,in T6>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6);
    public delegate void CallsiteAction<in T1,in T2,in T3,in T4,in T5,in T6,in T7>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7);
    public delegate void CallsiteAction<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8);
    public delegate void CallsiteAction<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9);
    public delegate void CallsiteAction<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10);
    public delegate void CallsiteAction<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11);
    public delegate void CallsiteAction<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12);
    public delegate void CallsiteAction<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13);
    public delegate void CallsiteAction<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14);
    public delegate void CallsiteAction<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15);
    public delegate void CallsiteAction<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16);
    public delegate void CallsiteAction<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17);
    public delegate void CallsiteAction<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17,in T18>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17, T18 arg18);
    public delegate void CallsiteAction<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17,in T18,in T19>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17, T18 arg18, T19 arg19);
    public delegate void CallsiteAction<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17,in T18,in T19,in T20>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17, T18 arg18, T19 arg19, T20 arg20);

    // delegates that take a ref first argument
    // used for instance methods of value types that return non-void
    public delegate TResult CallsiteFuncRef<T1,out TResult>(ref T1 arg1);
    public delegate TResult CallsiteFuncRef<T1,in T2,out TResult>(ref T1 arg1, T2 arg2);
    public delegate TResult CallsiteFuncRef<T1,in T2,in T3,out TResult>(ref T1 arg1, T2 arg2, T3 arg3);
    public delegate TResult CallsiteFuncRef<T1,in T2,in T3,in T4,out TResult>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4);
    public delegate TResult CallsiteFuncRef<T1,in T2,in T3,in T4,in T5,out TResult>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5);
    public delegate TResult CallsiteFuncRef<T1,in T2,in T3,in T4,in T5,in T6,out TResult>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6);
    public delegate TResult CallsiteFuncRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,out TResult>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7);
    public delegate TResult CallsiteFuncRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,out TResult>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8);
    public delegate TResult CallsiteFuncRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,out TResult>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9);
    public delegate TResult CallsiteFuncRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,out TResult>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10);
    public delegate TResult CallsiteFuncRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,out TResult>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11);
    public delegate TResult CallsiteFuncRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,out TResult>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12);
    public delegate TResult CallsiteFuncRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,out TResult>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13);
    public delegate TResult CallsiteFuncRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,out TResult>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14);
    public delegate TResult CallsiteFuncRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,out TResult>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15);
    public delegate TResult CallsiteFuncRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,out TResult>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16);
    public delegate TResult CallsiteFuncRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17,out TResult>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17);
    public delegate TResult CallsiteFuncRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17,in T18,out TResult>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17, T18 arg18);
    public delegate TResult CallsiteFuncRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17,in T18,in T19,out TResult>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17, T18 arg18, T19 arg19);
    public delegate TResult CallsiteFuncRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17,in T18,in T19,in T20,out TResult>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17, T18 arg18, T19 arg19, T20 arg20);
    
    // delegates that take a ref first argument, return void
    // used for instance methods of value types that return void
    public delegate void CallsiteActionRef<T1>(ref T1 arg1);
    public delegate void CallsiteActionRef<T1,in T2>(ref T1 arg1, T2 arg2);
    public delegate void CallsiteActionRef<T1,in T2,in T3>(ref T1 arg1, T2 arg2, T3 arg3);
    public delegate void CallsiteActionRef<T1,in T2,in T3,in T4>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4);
    public delegate void CallsiteActionRef<T1,in T2,in T3,in T4,in T5>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5);
    public delegate void CallsiteActionRef<T1,in T2,in T3,in T4,in T5,in T6>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6);
    public delegate void CallsiteActionRef<T1,in T2,in T3,in T4,in T5,in T6,in T7>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7);
    public delegate void CallsiteActionRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8);
    public delegate void CallsiteActionRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9);
    public delegate void CallsiteActionRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10);
    public delegate void CallsiteActionRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11);
    public delegate void CallsiteActionRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12);
    public delegate void CallsiteActionRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13);
    public delegate void CallsiteActionRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14);
    public delegate void CallsiteActionRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15);
    public delegate void CallsiteActionRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16);
    public delegate void CallsiteActionRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17);
    public delegate void CallsiteActionRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17,in T18>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17, T18 arg18);
    public delegate void CallsiteActionRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17,in T18,in T19>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17, T18 arg18, T19 arg19);
    public delegate void CallsiteActionRef<T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17,in T18,in T19,in T20>(ref T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17, T18 arg18, T19 arg19, T20 arg20);
    
    public static partial class DelegateHelpers
    {
        // high performance well typed delegate
        // fast but require combinatorial explosion of compiler generated
        // dummy code to get around generic limitations on il2cpp
        // here be dragons...

        // public static T CreateDelegate<T>(MethodInfo method) where T : Delegate
        // {
        //     return (T)Delegate.CreateDelegate(typeof(T), null, method);
        // }
        // public static Func<object, object> GetDelegateWeakFromStrong01<A, Z>(MethodInfo method)
        // {
        //     var func = CreateDelegate<Func<A, Z>>(method);
        //     Func<object, object> ret = a => func((A)a);
        //     return ret;
        // }
        // public static Func<object, object, object> GetDelegateWeakFromStrong02<A, B, Z>(MethodInfo method)
        // {
        //     Console.WriteLine("[GetDelegateWeakFromStrong02] {0} {1} {2} {3}", method, typeof(A), typeof(B), typeof(Z));
        //     var func = CreateDelegate<Func<A, B, Z>>(method);
        //     Func<object, object, object> ret = (a, b) => func((A)a, (B)b);
        //     return ret;
        // }
        // public static Func<object, object, object, object> GetDelegateWeakFromStrong03<A, B, C, Z>(MethodInfo method)
        // {
        //     var func = CreateDelegate<Func<A, B, C, Z>>(method);
        //     Func<object, object, object, object> ret = (a, b, c) => func((A)a, (B)b, (C)c);
        //     return ret;
        // }

        public static CallsiteFunc<object, object> GetZeroArityDelegate(FieldInfo field)
        {
            return obj => field.GetValue(obj);
        }

        public static CallsiteFunc<object, object> GetZeroArityDelegate(MethodInfo method)
        {
            return DelegateHelpers.GetMethodDelegate01(method);
        }

        public static CallsiteFunc<object, object> GetZeroArityDelegate(PropertyInfo property)
        {
            return GetZeroArityDelegate(property.GetGetMethod());
        }

        public static CallsiteFunc<object, object, object> GetSetMemberDelegate(FieldInfo field)
        {
            if(field.FieldType.IsPrimitive)
                return (obj, val) => {
                    val = Binder.Shared.ConvertArgument(field.FieldType, val);
                    field.SetValue(obj, val);
                    return val;
                };
            else
                return (obj, val) => {
                    val = Binder.Shared.ConvertArgument(field.FieldType, val);
                    field.SetValue(obj, val);
                    return val;
                };
        }

        public static CallsiteFunc<object, object, object> GetSetMemberDelegate(PropertyInfo property)
        {
            if(property.PropertyType.IsPrimitive)
                return (obj, val) => {
                    val = Binder.Shared.ConvertArgument(property.PropertyType, val);
                    property.SetValue(obj, val, null);
                    return val;
                };
            else
                return (obj, val) => {
                    val = Binder.Shared.ConvertArgument(property.PropertyType, val);
                    property.SetValue(obj, val, null);
                    return val;
                };
        }
    }

    // (set! (.name o) value)
    public class CallSiteSetMember
    {
        string MemberName;
        // TODO not actually overloaded on value type
        // might need custom cache type
        CallSiteCache02 cache;

        public CallSiteSetMember(string memberName)
        {
            MemberName = memberName;
            cache = new CallSiteCache02();
        }

        public object Invoke(object o, object value)
        {
            if(cache.TryGet(o, value, out var result))
                return result(o, value);

            CallsiteFunc<object, object, object> f = null;
            
            var oType = o == null ? typeof(Object) : o.GetType();
            var field = oType.GetField(MemberName);
            if (field != null)
            {
                f = DelegateHelpers.GetSetMemberDelegate(field);
            }
            var property = oType.GetProperty(MemberName);
            if (property != null)
            {
                f = DelegateHelpers.GetSetMemberDelegate(property);
            }
            if(f != null)
            {
                cache.CacheMethod(o, value, f);
                return f(o, value);
            }
            throw new ArgumentException($"Could not set member `{MemberName}` on target {o.ToString()}, no such member exists.");
        }
    }

    // (.name o)
    public class CallSiteZeroArityMember
    {
        string MemberName;
        CallSiteCache01 cache;

        public CallSiteZeroArityMember(string memberName)
        {
            MemberName = memberName;
            cache = new CallSiteCache01();
        }

        public object Invoke(object o)
        {
            if(cache.TryGet(o, out var result)) {
                return result(o);
            }
            
            CallsiteFunc<object, object> f = null;
            
            var oType = o == null ? typeof(Object) : o.GetType();
            var field = oType.GetField(MemberName);
            if (field != null)
            {
                f = DelegateHelpers.GetZeroArityDelegate(field);
            }
            var property = oType.GetProperty(MemberName);
            if (property != null)
            {
                f = DelegateHelpers.GetZeroArityDelegate(property);
            }
            var method = oType.GetMethod(MemberName, Type.EmptyTypes);
            if (method != null)
            {
                f = DelegateHelpers.GetZeroArityDelegate(method);
            }
            if(f != null)
            {
                cache.CacheMethod(o, f);
                return f(o);
            }
            throw new ArgumentException($"Could not invoke zero arity member `{MemberName}` on target {(o == null ? "null" : o.ToString())}.");
        }
    }
}