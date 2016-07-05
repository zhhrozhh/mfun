#ifndef ALGEBRAH
#define ALGEBRAH
#include<iostream>
#include<vector>
#include<string>
#include<sstream>
#include"unifunc.h"
using namespace std;
template<class T>class MAT;
template<class T>class PLU;
template<class T>bool abscmpG(T a,T b){
    return a*a>b*b;
}
template<class T>MAT<T>idMAT(size_t a,T nonzero){
    T id = nonzero/nonzero;
    T nu = nonzero-nonzero;
    vector<vector<T> >res;
    vector<T>r1;
    for(size_t i=0;i<a;i++){
        for(size_t j=0;j<a;j++){
            if(j==i)r1.push_back(id);
            else r1.push_back(nu);
        }
        res.push_back(r1);
        r1 = vector<T>();
    }
    return MAT<T>(res);
}
template<class T>MAT<T>square(size_t a,T ele){
    vector<vector<T> >res;
    vector<T>r;
    for(size_t i=0;i<a;i++){
        r = vector<T>();
        for(size_t j=0;j<a;j++)r.push_back(ele);
        res.push_back(r);
    }
    return MAT<T>(res);
}
template<class T>class PLU{
    public:
        PLU():P(MAT<T>()),L(MAT<T>()),U(MAT<T>()),neg(false){}
        MAT<T>P;
        MAT<T>L;
        MAT<T>U;
        bool neg;
        void disp(){
            P.disp();
            L.disp();
            U.disp();
        }
};
template<class T>class MAT{
    public:
        vector<vector<T> >m;
        vector<T>tem;
        bool err;
        MAT():m(vector<vector<T> >()),err(1){};
        MAT(vector<vector<T> >a):m(a),err(0){};
        MAT(vector<T>a):err(0){
            m.push_back(a);
        }
        MAT(string s){
            vector<string>es = split(s,';');
            for(size_t i=0;i<es.size();i++){
                T item;
                istringstream iss(es[i]);
                while(iss>>item)tem.push_back(item);
                m.push_back(tem);
                tem.clear();
            }
        }
        size_t r(){return m.size();}
        size_t c(){return m[0].size();}
        MAT<T>operator*(T oth){
            if(err)return MAT<T>();
            vector<vector<T> >res;
            vector<T>r1;
            for(size_t i=0;i<r();i++){
                r1=vector<T>();
                for(size_t j=0;j<c();j++)
                    r1.push_back(m[i][j]*oth);
                res.push_back(r1);
            }
            return MAT<T>(res);
        }

        T sum(){
            T sum = (T)0;
            for(size_t i=0;i<r();i++)
                for(size_t j=0;j<c();j++)
                    sum = sum + m[i][j];
            return sum;
        }
        MAT<T>operator/(T oth){
            if(err)return MAT<T>();
            vector<vector<T> >res;
            vector<T>r1;
            for(size_t i=0;i<r();i++){
                r1=vector<T>();
                for(size_t j=0;j<c();j++)
                    r1.push_back(m[i][j]/oth);
                res.push_back(r1);
            }
            return MAT<T>(res);
        }
        MAT<T>operator+(MAT<T>oth){
            size_t tr = r();
            size_t tc = c();
            size_t hr = oth.r();
            size_t hc = oth.c();
            if(err||oth.err||(tr-hr)||(tc-hc))return MAT<T>();
            vector<vector<T> >res;
            vector<T>r1;
            for(size_t i=0;i<tr;i++){
                r1 = vector<T>();
                for(size_t j=0;j<tc;j++)
                    r1.push_back(m[i][j]+oth.m[i][j]);
                res.push_back(r1);
            }
            return MAT(res);
        }
        MAT<T>operator-(MAT<T>oth){
            size_t tr = r();
            size_t tc = c();
            size_t hr = oth.r();
            size_t hc = oth.c();
            if(err||oth.err||(tr-hr)||(tc-hc))return MAT<T>();
            vector<vector<T> >res;
            vector<T>r1;
            for(size_t i=0;i<tr;i++){
                r1 = vector<T>();
                for(size_t j=0;j<tc;j++)
                    r1.push_back(m[i][j]-oth.m[i][j]);
                res.push_back(r1);
            }
            return MAT(res);
        }
        MAT<T>operator*(MAT<T>oth){

            size_t tr = r();
            size_t hc = oth.c();
            size_t tc = c();
            size_t hr = oth.r();
            if(err||oth.err||(tc-hr))return MAT<T>();
            vector<vector<T> >res;
            vector<T>r1;
            T sum = m[0][0] + oth.m[0][0];
            for(size_t i=0;i<tr;i++){
                r1 = vector<T>();
                for(size_t j=0;j<hc;j++){
                    sum = m[i][0] * oth.m[0][j];
                    for(size_t k=1;k<tc;k++)sum = sum + m[i][k]*oth.m[k][j];
                    r1.push_back(sum);
                }
                res.push_back(r1);
            }
            return MAT(res);
        }
        bool operator==(MAT<T>oth){
            if(r()!=oth.r() || c()!=oth.c())return false;
            for(size_t i=0;i<r();i++)
                for(size_t j=0;j<c();j++)
                    if(m[i][j]!=oth.m[i][j])return false;
            return true;
        }
        vector<T>operator[](size_t i){
            return m[i];
        }
        MAT<T>t(){
            if(err)return MAT();
            vector<vector<T> >res;
            vector<T>r1;
            for(size_t i=0;i<c();i++){
                r1 = vector<T>();
                for(size_t j=0;j<r();j++)
                    r1.push_back(m[j][i]);
                res.push_back(r1);
            }
            return MAT(res);
        }
        MAT<T>minor(size_t a,size_t(b)){
            if((a>r() )||( b>c() )|| err)return MAT();
            vector<vector<T> >res;
            vector<T>r1;
            for(size_t i=0;i<r();i++){
                if(i-a){
                    r1 = vector<T>();
                    for(size_t j=0;j<c();j++)if(j-b)r1.push_back(m[i][j]);
                    res.push_back(r1);
                }
            }
            return MAT<T>(res);
        }
        T det(){
            PLU<T>res = PLUdecomp();
            T sum=res.L.m[0][0];
            for(size_t i=1;i<res.L.r();i++)sum = sum*res.L.m[i][i];
            return res.neg?-sum:sum;
        }
        MAT<T>row(size_t rn){
            if(rn>r())return MAT<T>();
            vector<vector<T> >res;
            res.push_back(vector<T>());
            for(size_t i=0;i<m[rn].size();i++)
                res[0].push_back(m[rn][i]);
            return MAT<T>(res);
        }
        MAT<T>col(size_t cn){
            if(cn>c())return MAT<T>();
            vector<vector<T> >res;
            for(size_t i=0;i<m.size();i++){
                vector<T> tem;
                tem.push_back(m[i][cn]);
                res.push_back(tem);
            }
            return MAT<T>(res);
        }
        MAT<T>inv(){
            PLU<T>res = PLUdecomp();
            T id = res.U.m[0][0];
            MAT<T>Y(res.P.m);
            MAT<T>X = square<T>(r(),id);
            for(size_t i=0;i<r();i++)X.m[0][i]=Y.m[0][i]/res.L.m[0][0];
            for(size_t i=1;i<r();i++)
                for(size_t j=0;j<r();j++){
                    T sum = res.L.m[i][0]*X.m[0][j];
                    for(size_t k=1;k<i;k++)sum = sum + res.L.m[i][k]*X.m[k][j];
                    X.m[i][j]=(Y.m[i][j]-sum)/res.L.m[i][i];
                }
            for(size_t i=0;i<r();i++)Y.m[r()-1][i] = X.m[r()-1][i];
            for(size_t i=r()-2;i+1;i--)
                for(size_t j=0;j<r();j++){
                    T sum = res.U.m[i][i+1]*Y.m[i+1][j];
                    for(size_t k=i+2;k<r();k++)sum = sum+res.U.m[i][k]*Y.m[k][j];
                    Y.m[i][j]=X.m[i][j]-sum;
                }
            return Y;
        }
        MAT<T>switchCol(size_t a,size_t b){
            vector<vector<T> >res = m;
            for(size_t i=0;i<c();i++){
                res[i][a]=m[i][b];
                res[i][b]=m[i][a];
            }
            return MAT<T>(res);
        }
        MAT<T>switchRow(size_t a,size_t b){
            vector<vector<T> >res = m;
            res[a]=m[b];
            res[b]=m[a];
            return MAT<T>(res);
        }
        MAT<T>rowMul(size_t a,T c){
            vector<vector<T> >res = m;
            for(size_t i=0;i<c();i++)res[a][i]=c*res[a][i];
            return MAT<T>(res);
        }
        MAT<T>addRowTo(size_t a,size_t b){
            vector<vector<T> >res = m;
            for(size_t i=0;i<c();i++)res[b][i]=res[b][i]+res[a][i];
            return MAT<T>(res);
        }
        PLU<T>PLUdecomp(){
            PLU<T>res = PLU<T>();
            T nonzero = (T)1;
            if((r()-c()))return res;
            res.P = idMAT<T>(r(),nonzero);
            res.L = MAT<T>(res.P.m);
            res.U = MAT<T>(m);
            bool neg;
            for(size_t i=0;i<r();i++){
                size_t max = i;
                for(size_t j=i+1;j<r();j++)
                    if(abscmpG(res.U.m[j][i],res.U.m[max][i]))max = j;
                if(max-i){
                    res.P = res.P.switchRow(i,max);
                    res.U = res.U.switchRow(i,max);
                    res.neg = !res.neg;
                }
            }
            for(size_t i=0;i<r();i++){
                res.L.m[i][i] = res.U.m[i][i];
                for(size_t j=i;j<r();j++)res.U.m[i][j] = res.U.m[i][j]/res.L.m[i][i];
                for(size_t j=i+1;j<r();j++){
                    res.L.m[j][i] = res.U.m[j][i];
                    for(size_t k=0;k<r();k++)
                        res.U.m[j][k] = res.U.m[j][k]-res.L.m[j][i]*res.U.m[i][k];
                }
            }
            return res;
        }
        void disp(){
            cout<<endl;
            size_t x = r();
            size_t y = c();
            for(size_t i=0;i<x;i++){
                for(size_t j=0;j<y;j++)cout<<m[i][j]<<"\t";
                cout<<endl;
            }
            cout<<endl;
        }
};

MAT<double>iddMAT(size_t a){
    return idMAT<double>(a,1);
}

template<class T>MAT<T>ptwiseMul(MAT<T>a,MAT<T>b){
    vector<vector<T> >res;
    for(size_t i=0;i<a.r();i++){
        vector<T>tem;
        for(size_t j=0;j<a.c();j++)
            tem.push_back(a.m[i][j]*b.m[i][j]);
        res.push_back(tem);
    }
    return MAT<T>(res);
}
template<class T>MAT<T>ptwiseAdd(MAT<T>a,MAT<T>b){
    vector<vector<T> >res;
    for(size_t i=0;i<a.r();i++){
        vector<T>tem;
        for(size_t j=0;j<a.c();j++)
            tem.push_back(a.m[i][j]+b.m[i][j]);
        res.push_back(tem);
    }
    return MAT<T>(res);
}
#endif


