#include"mfun.h"
#define FFERR FF(new MfunErr())
class FF{
    public:
        FF(Mfun*f):f(f){}
        FF(double d):f(new MfunCons(d)){}
        FF operator+(FF oth){
            return FF(ADD(f,oth.f));
        }
        FF operator-(FF oth){
            return FF(SUB(f,oth.f));
        }
        FF operator*(FF oth){
            return FF(MUL(f,oth.f));
        }
        FF operator/(FF oth){
            return FF(DIV(f,oth.f));
        }
        FF operator^(FF oth){
            return POW(f,oth.f);
        }
        FF grad(){
            return FF(f->grad());
        }
        FF trace(){
            if(f->type=="m")return FF(((MfunMat*)f)->trace());
            return FFERR;
        }
        FF d(string v){
            return FF(f->d(v));
        }
        FF getVal(map<string,double>&m){
            return FF(f->getVal(m));
        }
        void varScan(map<string,double>&m){
            f->varScan(m);
        }
        void addRow(vector<Mfun*>row){
            if(f->type!="m"){
                vector<vector<Mfun*> >res;
                vector<Mfun*>r1;
                r1.push_back(f);
                res.push_back(r1);
                res.push_back(row);
                f = new MfunMat(res);
                return;
            }
            ((MfunMat*)f)->addRow(row);
        }
        void addRow(FF*a0,...){
            if(f->type!="m")return;
            vector<Mfun*>row;
            row.push_back(a0->f);
            va_list vl;
            va_start(vl,a0);
            for(size_t i = 1;i<((MfunMat*)f)->size.c;i++)
                row.push_back(va_arg(vl,FF*)->f);
            va_end(vl);
            ((MfunMat*)f)->addRow(row);
        }
        void addCol(vector<Mfun*>col){
            if(f->type!="m")return;
            ((MfunMat*)f)->addCol(col);
        }
        void addCol(FF*a0,...){
            if(f->type!="m")return;
            va_list vl;
            vector<Mfun*>col;
            va_start(vl,a0);
            col.push_back(a0->f);
            for(size_t i=0;i<((MfunMat*)f)->size.r;i++)
                col.push_back(va_arg(vl,FF*)->f);
            va_end(vl);
            ((MfunMat*)f)->addCol(col);

        }
        string expr(){
            return f->ts();
        }
        Mfun*f;
};
#define F__(x) \
    FF x(FF y){\
        return FF(M##x(y.f));\
    }
F__(Sin);
F__(Cos);
F__(Tan);
F__(Cot);
F__(Sec);
F__(Csc);
F__(Sinh);
F__(Cosh);
F__(Tanh);
F__(Coth);
F__(Sech);
F__(Csch);
F__(ArcTan);
FF var(string x){
    return FF(new MfunVar(x));
}
#undef F__
