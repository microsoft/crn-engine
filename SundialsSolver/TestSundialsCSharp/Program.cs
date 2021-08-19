using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace TestSundialsCSharp
{
    static class Native
    {
        [DllImport("SundialsSolver15.dll", CallingConvention = CallingConvention.Cdecl)]
        internal static extern int fnSundialsSolver(int sN, int rN, int[] stoich, double[] rates, double[] x0, double dt,
                                            double[] sol, int numtimepoints, int[] indicesToReturn, int numIndicesToReturn,
                                            bool stiff);
    }
    
    class Program
    {
        static void Main(string[] args)
        {

            const int sN = 4;
	        const int rN = 2;
	        const double tfinal = 10.0;
	        const int timeSamples = 1;

	        var stoich = new int[sN*rN];
	        stoich[0] = -1;
	        stoich[1] = 1;
	        stoich[2] = 1;
	        stoich[3] = -1;
	        stoich[4] = 1;
	        stoich[5] = -1;
	        stoich[6] = -1;
	        stoich[7] = 1;

	        var rates = new double[rN];
	        rates[0] = 0.1;
	        rates[1] = 0.01;

	        var x0 = new double[sN];
	        x0[0] = 0.0;
	        x0[1] = 100.0;
	        x0[2] = 100.0;
	        x0[3] = 0.0;

	        var indicesToReturn = new[] {0,1};

            var sol = new double[sN * (timeSamples + 1)];

            /*Parallel.For(0, 500000, (x) => Native.fnSundialsSolver(sN, rN, stoich, rates, x0, tfinal, sol,
                                                                   timeSamples, indicesToReturn,
                                                                   indicesToReturn.Length, false));*/

	        for (int i = 0; i < 100000; i++) {
                Native.fnSundialsSolver(sN, rN, stoich, rates, x0, tfinal, sol, timeSamples, indicesToReturn, indicesToReturn.Length, false);
	        }
        }
    }
}
