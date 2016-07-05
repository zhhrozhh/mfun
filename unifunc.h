#ifndef UNIFUNC_H
#define UNIFUNC_H


#include<stdio.h>
#include<stdlib.h>
#include<iostream>
#include<string>
#include<sstream>
#include<algorithm>
#define MAX(a,b) ((a>b)?a:b)
#define DBG cout<<__LINE__<<endl
using namespace std;
void cursorTo(size_t x,size_t y){
    printf("%c[%lu;%luf",0x1B,y,x);
}

char getChar(){
    system("/bin/stty raw");
    char res=getchar();
    system("/bin/stty cooked");
    return res;
}

vector<string> split(string s, char delim) {
    vector<string>elems;
    stringstream ss(s);
    string item;
    while (getline(ss, item, delim))
        elems.push_back(item);
    return elems;
}

template<class T>T pow(T x,size_t n){
    if(!n)return (T)1;
        T rslt=(T)1;
        while(n){
            if(n & 1)rslt *= x;
            x *= x;
            n >>= 1;
        }
        return rslt;
}
template<class T>vector<T>cyclicRShift(vector<T>v){
    vector<T>res;
    res.push_back(v.back());
    for(size_t i=0;(int)i<((int)(v.size())-1);i++)res.push_back(v[i]);
    return res;
}
template<class T>vector<T>cyclicLShift(vector<T>v){
    vector<T>res;
    for(size_t i=1;i<v.size();i++)res.push_back(v[i]);
    res.push_back(v[0]);
    return res;
}
template<class T>vector<T>cyclicShift(vector<T>v,bool right,size_t step){
    vector<T>res = v;
    for(size_t i=0;i<step;i++)
        res = right?cyclicRShift<T>(res):cyclicLShift<T>(res);
    return res;
}
template<class T>vector<T>reverse(vector<T>v){
    vector<T>cpy = v;
    reverse(cpy.begin(),cpy.end());
    return cpy;

}
#endif


