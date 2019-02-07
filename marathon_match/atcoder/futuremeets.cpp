#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

struct Rand {
	uint32_t x,y,z,w;
	static constexpr double iDouble=1.0/(1LL<<32);
	Rand() {
		x=123456789;y=362436069;z=521288629;w=88675123;
	}
	uint32_t nextInt(uint32_t n) {
		uint32_t t=x^(x<<11);
		x=y;y=z;z=w;
		w=(w^(w>>19))^(t^(t>>8));
		return w%n;
	}
	uint32_t nextInt() {
		uint32_t t=x^(x<<11);
		x=y;y=z;z=w;
		return w=(w^(w>>19))^(t^(t>>8));
	}
	double nextDouble() {return nextInt()*iDouble;}
} rnd;

double timeLimit=4.90;
const int64_t CYCLES_PER_SEC=2800000000;
struct Timer {
	int64_t start;
	Timer() { reset(); }
	void reset() { start = getCycle(); }
	void plus(double a) { start -= (a * CYCLES_PER_SEC); }
	inline double get() { return (double)(getCycle() - start) / CYCLES_PER_SEC; }
	inline int64_t getCycle() {
		uint32_t low, high;
		__asm__ volatile ("rdtsc" : "=a" (low), "=d" (high));
		return ((int64_t)low) | ((int64_t)high << 32);
	}
};

int N,K;
int a[30001];
int score,bestscore;
int ri[3001],rj[3001],rk[3001],rl[3001],rv[3001];
int sa[30001];
const int INF=1000100010;
int main(){
    cin>>N>>K;
    rep(i,N) cin>>a[i],sa[i]=a[i]-i-1;
    rep(i,K){
        int bl,br,bg=0,btv=0;
        rep(ii,N) sa[ii+1]=a[ii]-ii-1+sa[ii];
        FOR(rg,1,11){
            int l,r;
            int mn=INF,mx=-INF;
            
            rep(j,N-rg){
                int sum=sa[j+rg]-sa[j];
                /*
                rep(k,rg){
                    sum+=a[j+k]-j-k-1;
                }
                */
                if(mn>sum) r=j,mn=sum;
                if(mx<sum) l=j,mx=sum;
            }
            int tv=INF;
            rep(j,rg){
                tv=min(tv,min(a[l+j]-1,N-a[r+j]));
            }
            tv=min(tv,(mx-mn)/(2*rg));
            if(btv*bg<=tv*rg){
                bl=l,br=r,bg=rg,btv=tv;
            }
        }
        ri[i]=bl,rj[i]=bl+bg-1,rk[i]=br,rl[i]=br+bg-1,rv[i]=btv;
        rep(j,bg){
            a[bl+j]-=btv;
            a[br+j]+=btv;
        }
    }
    //max,min,sa
    

    rep(i,K){
        cout<<ri[i]+1<<" "<<rj[i]+1<<" "<<rk[i]+1<<" "<<rl[i]+1<<" "<<rv[i]<<endl;
    }
    return 0;
}
