/**
 * Calculate the velocity autocorrelation functions of an AdResS system using block average
 *
 */
#include <algorithm>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <list>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <queue>
#include <vector>
#include <cmath>


#include "xdrfile/xdrfile.h"
#include "xdrfile/xdrfile_trr.h"
#include <boost/program_options.hpp>
namespace po = boost::program_options;



int main(int argc, char *argv[])
{
 int N_frame,  N_total, AtomperMol, numframes;
 int m, n, i, j, k, data_count;
 int step, Natoms, *count, N_acc, nmolecules;
 float Delta_t, time, lambda, *vel_acc, temp, x0, x1, begin, end;
 matrix box;
 rvec **x, **v, *f;
 std::string ifile, ofile, method;  
 po::options_description desc ("Allow options");
 desc.add_options()
      ("help,h", "print this message")
      ("begin,b", po::value<float > (&begin)->default_value(0.f), "start time")
      ("end,e",   po::value<float > (&end  )->default_value(0.f), "end   time")
      ("x0", po::value<float > (&x0)->default_value(0.f), "lower bound of the interval")
      ("x1", po::value<float > (&x1)->default_value(1.f), "upper bound of the interval, if x1 == 0, use the whole box")
      ("frame",   po::value<int > (&N_frame)->default_value(50), "length of correlation")
      ("acc",   po::value<int > (&N_acc)->default_value(2000), "breaks")
      ("total", po::value<int > (&N_total)->default_value(100000), "number of frames" )
      ("tf", po::value<float > (&Delta_t)->default_value(1.f), "Output Frequency")
      ("method,m",  po::value<std::string > (&method)->default_value ("adress"), "type of simulation to analyze")
      ("input,f",   po::value<std::string > (&ifile)->default_value ("traj.trr"), "the input .xtc file")
      ("output,o",  po::value<std::string > (&ofile)->default_value ("vac.dat"), "the output file");

  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, desc), vm);
  po::notify (vm);
  if (vm.count("help")){
    std::cout << desc<< "\n";
    return 0;
  }

 XDRFILE * trrfile;
 char tmpfname[1024];
 strncpy (tmpfname, ifile.c_str(), 1023);
 trrfile = xdrfile_open(tmpfname, "r");
 read_trr_natoms(tmpfname, &Natoms);

 if (method == std::string("adress")){
    AtomperMol=4;
    nmolecules = Natoms / 4;
  }
  else if (method == std::string("atom")){
    AtomperMol=3;
    nmolecules = Natoms / 3;
  }
 
 printf("# total %d atoms,   %d molecules \n", Natoms, nmolecules);
 
 x = (rvec **) malloc (sizeof(rvec*)*N_acc);
 v = (rvec **) malloc (sizeof(rvec*)*N_acc);

  //f = (rvec **) malloc (sizeof(rvec*)*N_total);
 vel_acc = (float *) calloc (N_frame, sizeof(float));
 count   = (int   *) calloc (N_frame, sizeof(int));

//  vel_acc_list=(float *) calloc (Natoms*N_total, sizeof(float));
 for(i=0;i<N_acc;i++)
    {
      x[i] = (rvec *) calloc (Natoms,sizeof(rvec)); 
      v[i] = (rvec *) calloc (Natoms,sizeof(rvec));
      //f[i] = (rvec *) calloc (Natoms,sizeof(rvec)); 
    }
  
  // read data
 data_count=0;
 int fac = N_total/N_acc;
  
 for (j = 0; j < fac; j++)
  {
   i = 0;
   while(read_trr(trrfile, Natoms, &step, &time, &lambda, box, x[i], v[i], f)==0)
     {      
       printf("%d \n", N_acc * j + i);
       i++;
       if (i == N_acc)
         {
             for (k = 0; k < N_frame; k++)
              {
                for (m = 0; m < N_acc; m++)
                 {
                  for (n = 0; n < Natoms; n++)
                   {
                    if (n%AtomperMol == 0)
                     {
                      if ((m + k) < N_acc)
                      { 
                      if (x[m][n][0] > x0 && x[m][n][0] < x1 && x[m+k][n][0] > x0 && x[m+k][n][0] < x1)
                      { 
                        
                           count[k]++;
                           vel_acc[k] += v[m][n][0] * v[m + k][n][0] + v[m][n][1] * v[m + k][n][1] + v[m][n][2] * v[m+k][n][2];
                         
                      }
                      }
                     } 
                   }
                 }
              }      
         break;
         }
     }
 }
  
  free(x);
  free(v);
  xdrfile_close(trrfile);
 

  FILE *fout = fopen (ofile.c_str(), "w");
  if (fout == NULL){
    std::cerr << "cannot open file " << ofile << std::endl;
    exit (1);
  }
   
  float norm;
 
  for (k = 0; k < N_frame; k++)
   {
       vel_acc[k]/=count[k];
   }    
   norm = vel_acc[0];
  
  for (k = 0; k < N_frame; k++)
   {
       vel_acc[k]/=norm;
       fprintf(fout, "%f %f \n", Delta_t*k, vel_acc[k]);
   }    
 
fclose(fout);
return 0;

}
