#include<iostream>
#include<vector>
#include<string>
#include<map>
#include<cmath>
using namespace std;
class Mfun{
    public:
        class DnC{//double value + check bit
            public:
                DnC():d(0),c(0){}
                DnC(double d,bool c):d(d),c(c){}
                double d;
                bool c;
        };
        Mfun(string type):type(type){}
        virtual Mfun*d(string var){return nullptr;}
        virtual Mfun*x1(){return nullptr;}
        virtual Mfun*x2(){return nullptr;}
        virtual void varScan(map<string,DnC>&m){};
        virtual Mfun*grad();
        virtual Mfun*getVal(map<string,DnC>&m){return nullptr;}
        string type;
};
class MfunErr:public Mfun{
    public:
        MfunErr():Mfun("Err"){}
};
class MfunCons:public Mfun{
    public:
        MfunCons(double val):Mfun("c"),val(val){}
        Mfun*d(string var){return new MfunCons(0);}
        void varScan(map<string,DnC>&m){}
        Mfun*getVal(map<string,DnC>&m){
            return this;
        }
        double val;
};
class MfunVar:public Mfun{
    public:
        MfunVar(string var):Mfun("v"),var(var){}
        Mfun*d(string v){
            return new MfunCons(v==var);
        }
        void varScan(map<string,DnC>&m){
            if(m.find(var)==m.end())m.insert(make_pair(var,DnC()));
        }
        Mfun*getVal(map<string,DnC>&m){
            if(m[var].c)return new MfunCons(m[var].d);
            return new MfunErr();
        }
        string var;
};
class MfunUni:public Mfun{
    public:
        MfunUni(string type,Mfun*var1):Mfun(type),var1(var1){}
        Mfun*x1(){return var1;}
        void varScan(map<string,DnC>&m){
            var1->varScan(m);
        }
        Mfun*var1;
};
class MfunBi:public Mfun{
    public:
        MfunBi(string type,Mfun*var1,Mfun*var2):Mfun(type),var1(var1),var2(var2){}
        Mfun*x1(){return var1;}
        Mfun*x2(){return var2;}
        void varScan(map<string,DnC>&m){
            var1->varScan(m);
            var2->varScan(m);
        }
        Mfun*var1;
        Mfun*var2;
};
Mfun*ADD(Mfun*a,Mfun*b);
Mfun*SUB(Mfun*a,Mfun*b);
Mfun*MUL(Mfun*a,Mfun*b);
Mfun*DIV(Mfun*a,Mfun*b);
Mfun*POW(Mfun*a,Mfun*b);
Mfun*LN(Mfun*a);
class MfunMat:public Mfun{
    public:
        class SIZ{
            public:
                size_t r;
                size_t c;
                bool operator==(SIZ oth){
                    return (r==oth.r)&&(c==oth.c);
                }
        };
        MfunMat(vector<vector<Mfun*> >mat):Mfun("m"),mat(mat){
            size.r = mat.size();
            size.c = mat[0].size();
        }
        void varScan(map<string,DnC>&m){
            for(size_t i=0;i<size.r;i++)
                for(size_t j=0;j<size.c;j++)
                    mat[i][j]->varScan(m);
        }
        MfunMat*transpose(){
            //todo
            return nullptr;
        }
        vector<vector<Mfun*> >mat;
        Mfun*d(string v){
            vector<vector<Mfun*> >nv;
            for(size_t i=0;i<mat.size();i++){
                vector<Mfun*>vv;
                for(size_t j=0;j<mat[i].size();j++)
                    vv.push_back(mat[i][j]->d(v));
                nv.push_back(vv);
            }
            return new MfunMat(nv);
        }
        Mfun*trace(){
            if(size.c!=size.r)return new MfunErr();
            Mfun*res = new MfunCons(0);
            for(size_t i=0;i<size.c;i++)
                res = ADD(res,mat[i][i]);
            return res;
        }
        double det(vector<double>var){
            //todo
            return 0;
        }
        Mfun*getVal(map<string,DnC>&m){
            vector<vector<Mfun*> >res;
            for(size_t i=0;i<size.r;i++){
                vector<Mfun*>rv;
                for(size_t j=0;j<size.c;j++)
                    rv.push_back(mat[i][j]->getVal(m));
                res.push_back(rv);
            }
            return new MfunMat(res);
        }
        SIZ size;
};
class MfunAdd:public MfunBi{
    public:
        MfunAdd(Mfun*var1,Mfun*var2):MfunBi("+",var1,var2){}
        Mfun*d(string v){
            return ADD(var1->d(v),var2->d(v));
        }
        Mfun*getVal(map<string,DnC>&m){
            return ADD(var1->getVal(m),var2->getVal(m));
        }
};
class MfunSub:public MfunBi{
    public:
        MfunSub(Mfun*var1,Mfun*var2):MfunBi("-",var1,var2){}
        Mfun*d(string v){
            return SUB(var1->d(v),var2->d(v));
        }
        Mfun*getVal(map<string,DnC>&m){
            return SUB(var1->getVal(m),var2->getVal(m));
        }
};
class MfunMul:public MfunBi{
    public:
        MfunMul(Mfun*var1,Mfun*var2):MfunBi("*",var1,var2){}
        Mfun*d(string v){
            return ADD(MUL(var1->d(v),var2),MUL(var1,var2->d(v)));
        }
        Mfun*getVal(map<string,DnC>&m){
            return MUL(var1->getVal(m),var2->getVal(m));
        }
};
class MfunDiv:public MfunBi{
    public:
        MfunDiv(Mfun*var1,Mfun*var2):MfunBi("/",var1,var2){}
        Mfun*d(string v){
            return DIV(SUB(MUL(var1->d(v),var2),MUL(var1,var2->d(v))),MUL(var2,var2));
        }
        Mfun*getVal(map<string,DnC>&m){
            return DIV(var1->getVal(m),var2->getVal(m));
        }
};
class MfunPow:public MfunBi{
    public:
        MfunPow(Mfun*var1,Mfun*var2):MfunBi("^",var1,var2){}
        Mfun*d(string v){
            return ADD(MUL(MUL(var2,POW(var1,SUB(var2,new MfunCons(1)))),var1->d(v)),MUL(MUL(POW(var1,var2),LN(var1)),var2->d(v)));
        }
        Mfun*getVal(map<string,DnC>&m){
            return POW(var1->getVal(m),var2->getVal(m));
        }
};
Mfun*ADD(Mfun*a,Mfun*b){
    if((a->type=="c")&&(b->type=="c"))
        return new MfunCons(((MfunCons*)a)->val+((MfunCons*)b)->val);
    if((a->type=="c")||(b->type=="c")){
        MfunCons*c = (MfunCons*)((a->type=="c")?a:b);
        if(c->val==0)return (c==a)?b:a;
    }
    if((a->type=="m")&&(b->type=="m")){
        MfunMat*ma = (MfunMat*)a;
        MfunMat*mb = (MfunMat*)b;
        if(ma->size==mb->size){
            vector<vector<Mfun*> >vvm;
            for(size_t i=0;i<ma->size.r;i++){
                vector<Mfun*>vv;
                for(size_t j=0;j<ma->size.c;j++)
                    vv.push_back(ADD(ma->mat[i][j],mb->mat[i][j]));
                vvm.push_back(vv);
            }
            return new MfunMat(vvm);
        }
        return new MfunErr();
    }else if((a->type!="m")&&(b->type!="m")){
        return new MfunAdd(a,b);
    }else return new MfunErr();
}
Mfun*SUB(Mfun*a,Mfun*b){
    if((a->type=="c")&&(b->type=="c"))
        return new MfunCons(((MfunCons*)a)->val-((MfunCons*)b)->val);
    if((b->type=="c"&&(((MfunCons*)b)->val==0)))return a;
    if((a->type=="m")&&(b->type=="m")){
        MfunMat*ma = (MfunMat*)a;
        MfunMat*mb = (MfunMat*)b;
        if(ma->size==mb->size){
            vector<vector<Mfun*> >vvm;
            for(size_t i=0;i<ma->size.r;i++){
                vector<Mfun*>vv;
                for(size_t j=0;j<ma->size.c;j++)
                    vv.push_back(SUB(ma->mat[i][j],mb->mat[i][j]));
                vvm.push_back(vv);
            }
            return new MfunMat(vvm);
        }
        return new MfunErr();
    }else if((a->type!="m")&&(b->type!="m")){
        return new MfunSub(a,b);
    }else return new MfunErr();
}
Mfun*vecp(MfunMat*a,size_t i,MfunMat*b,size_t j){
    Mfun*res = new MfunCons(0);
    for(size_t k=0;k<a->size.c;k++)
        res = ADD(res,MUL(a->mat[i][k],b->mat[k][j]));
    return res;
}
Mfun*MUL(Mfun*a,Mfun*b){
    if((a->type=="c")&&(b->type=="c"))
        return new MfunCons((((MfunCons*)a)->val)*(((MfunCons*)b)->val));
    if((a->type=="c")||(b->type=="c")){
        MfunCons*c =(MfunCons*)((a->type=="c")?a:b);
        Mfun*d = (c==a)?b:a;
        if(c->val==1)return d;
        if(c->val==0)return new MfunCons(0);
    }
    if((a->type=="m")&&(b->type=="m")){
        MfunMat*ma = (MfunMat*)a;
        MfunMat*mb = (MfunMat*)b;
        if(ma->size.c==mb->size.r){
            vector<vector<Mfun*> >vvm;
            for(size_t i=0;i<ma->size.r;i++){
                vector<Mfun*>rv;
                for(size_t j=0;j<mb->size.c;j++)
                    rv.push_back(vecp(ma,i,mb,j));
                vvm.push_back(rv);
            }
            return new MfunMat(vvm);
        }
        return new MfunErr();
    }else if((a->type!="m")&&(b->type!="m")){
        return new MfunMul(a,b);
    }else{
        MfunMat*p = (MfunMat*)((a->type=="m")?a:b);
        Mfun*q = (q==a)?b:a;
        vector<vector<Mfun*> >vvm;
        for(size_t i=0;i<p->size.r;i++){
            vector<Mfun*>rv;
            for(size_t j=0;j<p->size.c;j++)
                rv.push_back(MUL(q,p->mat[i][j]));
            vvm.push_back(rv);
        }
        return new MfunMat(vvm);
    }
}
Mfun*DIV(Mfun*a,Mfun*b){
    if(b->type=="c")return MUL(a,new MfunCons(1/((MfunCons*)b)->val));
    else return new MfunDiv(a,b);
}
Mfun*matPow(Mfun*a,double b){
    //todo
    return a;
}
Mfun*POW(Mfun*a,Mfun*b){
    if(b->type=="m")return new MfunErr();
    if((a->type=="c")&&(b->type=="c"))
        return new MfunCons(pow(((MfunCons*)a)->val,((MfunCons*)b)->val));
    if((a->type=="m")&&(b->type=="c"))
        return matPow(a,((MfunCons*)b)->val);
    return new MfunPow(a,b);
}
Mfun* MSin(Mfun*);
Mfun* MCos(Mfun*);
Mfun* MTan(Mfun*);
Mfun* MCot(Mfun*);
Mfun* MSec(Mfun*);
Mfun* MCsc(Mfun*);
Mfun* MSinh(Mfun*);
Mfun* MCosh(Mfun*);
Mfun* MTanh(Mfun*);
Mfun* MCoth(Mfun*);
Mfun* MSech(Mfun*);
Mfun* MCsch(Mfun*);
class MfunExp:public MfunUni{
    public:
        MfunExp(Mfun*var1):MfunUni("exp",var1){}
        Mfun*d(string v){
            return MUL(this,var1->d(v));
        }
};
class MfunLn:public MfunUni{
    public:
        MfunLn(Mfun*var1):MfunUni("ln",var1){}
        Mfun*d(string v){
            return DIV(var1->d(v),var1);
        }
};
double gvSin(double x){return sin(x);}
double gvCos(double x){return cos(x);}
double gvTan(double x){return tan(x);}
double gvCot(double x){return sin(x)/cos(x);}
double gvSec(double x){return 1/cos(x);}
double gvCsc(double x){return 1/sin(x);}
double gvSinh(double x){return sinh(x);}
double gvCosh(double x){return cosh(x);}
double gvTanh(double x){return tanh(x);}
double gvCoth(double x){return sinh(x)/cosh(x);}
double gvSech(double x){return 1/cosh(x);}
double gvCsch(double x){return 1/sinh(x);}
#define UNI_ClassD(x) \
class Mfun##x:public MfunUni{\
    public:\
        Mfun##x(Mfun*var1):MfunUni(#x,var1){}\
        Mfun*d(string v);\
        Mfun*getVal(map<string,DnC>&m){\
            Mfun*tf = var1->getVal(m);\
            if(tf->type=="c")return new MfunCons(gv##x(((MfunCons*)tf)->val));\
            MfunMat*mm = (MfunMat*)tf;\
            vector<vector<Mfun*> >res;\
            for(size_t i=0;i<mm->size.r;i++){\
                vector<Mfun*>rv;\
                for(size_t j=0;j<mm->size.c;j++)\
                    rv.push_back(new MfunCons(gv##x(((MfunCons*)(mm->mat[i][j]))->val)));\
                res.push_back(rv);\
            }\
            return new MfunMat(res);\
        }\
}
UNI_ClassD(Sin);
UNI_ClassD(Cos);
UNI_ClassD(Tan);
UNI_ClassD(Cot);
UNI_ClassD(Sec);
UNI_ClassD(Csc);
UNI_ClassD(Sinh);
UNI_ClassD(Cosh);
UNI_ClassD(Tanh);
UNI_ClassD(Coth);
UNI_ClassD(Sech);
UNI_ClassD(Csch);
#define FuncD(x) \
Mfun* M##x(Mfun*v){\
    if(v->type=="m"){\
        vector<vector<Mfun*> >vvm;\
        MfunMat*m = (MfunMat*)v;\
        for(size_t i=0;i<m->size.r;i++){\
            vector<Mfun*>rv;\
            for(size_t j=0;j<m->size.c;j++)\
                rv.push_back(M##x(m->mat[i][j]));\
            vvm.push_back(rv);\
        }\
        return new MfunMat(vvm);\
    }\
    return new Mfun##x(v);\
}
FuncD(Sin);
FuncD(Cos);
FuncD(Tan);
FuncD(Cot);
FuncD(Sec);
FuncD(Csc);
FuncD(Sinh);
FuncD(Cosh);
FuncD(Tanh);
FuncD(Coth);
FuncD(Sech);
FuncD(Csch);
Mfun*LN(Mfun*var1){
    return new MfunLn(var1);
}
Mfun*MfunSin::d(string v){
    return MUL(MCos(var1),var1->d(v));
}
Mfun*MfunCos::d(string v){
    return MUL(MUL(new MfunCons(-1),MSin(var1)),var1->d(v));
}
Mfun*MfunTan::d(string v){
    return MUL(POW(MSec(var1),new MfunCons(2)),var1->d(v));
}
Mfun*MfunCot::d(string v){
    return MUL(MUL(new MfunCons(-1),POW(MCsc(var1),new MfunCons(2))),var1->d(v));
}
Mfun*MfunSec::d(string v){
    return MUL(MUL(MTan(var1),MSec(var1)),var1->d(v));
}
Mfun*MfunCsc::d(string v){
    return MUL(MUL(MUL(new MfunCons(-1),MCot(var1)),MCsc(var1)),var1->d(v));
}
Mfun*MfunSinh::d(string v){
    return MUL(MCosh(var1),var1->d(v));
}
Mfun*MfunCosh::d(string v){
    return MUL(MSinh(var1),var1->d(v));
}
Mfun*MfunTanh::d(string v){
    return MUL(POW(MSech(var1),new MfunCons(2)),var1->d(v));
}
Mfun*MfunCoth::d(string v){
    return MUL(MUL(new MfunCons(-1),MCsch(var1)),var1->d(v));
}
Mfun*MfunSech::d(string v){
    return MUL(MUL(MUL(MTanh(var1),MSech(var1)),new MfunCons(-1)),var1->d(v));
}
Mfun*MfunCsch::d(string v){
    return MUL(MUL(MUL(var1->d(v),MCsch(var1)),MCoth(var1)),new MfunCons(-1));
}
Mfun*Mfun::grad(){
    vector<Mfun*>res;
    map<string,DnC>varL;
    varScan(varL);
    for(map<string,DnC>::iterator it = varL.begin();it!=varL.end();it++)
        res.push_back(d(it->first));
    vector<vector<Mfun*> >rr;
    rr.push_back(res);
    return new MfunMat(rr);
}
