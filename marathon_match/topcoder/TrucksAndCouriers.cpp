#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

class TrucksAndCouriers {
public:

    vector<string> planShipping(int truckFixed,int truckVariable,
    vector<int> warehouseX,vector<int> warehouseY,vector<int> warehouseItem,
    vector<int> warehouseQuantity,vector<int> costomerX,vector<int> costomerY,vector<int> costmoerItem){

        vector<string> ans;
        return ans;
    }
};

// -------8<------- end of solution submitted to the website -------8<-------

template<class T> void getVector(vector<T>& v) {
    int sz;
    cin>>sz;
    v.resize(sz);
    rep(i,sz) cin>>v[i];
}

int main(){
    TrucksAndCouriers tc;
    int tF,tV;
    cin>>tF>>tV;
    vector<int> warehouseX,warehouseY,warehouseItem,
    warehouseQuantity,costomerX,costomerY,costmoerItem;
    getVector(warehouseX);
    getVector(warehouseY);
    getVector(warehouseItem);
    getVector(warehouseQuantity);
    getVector(costomerX);
    getVector(costomerY);
    getVector(costmoerItem);
    vector<string> ret=tc.planShipping(tF,tV,warehouseX,warehouseY,warehouseItem,
    warehouseQuantity,costomerX,costomerY,costmoerItem);
    cout<<ret.size()<<endl;
    rep(i,ret.size()) cout<<ret[i]<<endl;
    cout.flush();
    return 0;
}