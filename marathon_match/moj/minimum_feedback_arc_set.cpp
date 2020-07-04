#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;
typedef pair<double,int> pdi;

struct Rand2 {
	uint32_t y;
	static constexpr double iDouble=1.0/(1LL<<32);
	Rand2() {
		y=2463534242;
	}
	void setseed(uint32_t seed){y=seed;}
	uint32_t nextInt() {
    	return y^=(y^=(y^=y<<13)>>17)<<5;
	}
	uint32_t nextInt(uint32_t n) {
		return nextInt()%n;
	}
	double nextDouble() {return nextInt()*iDouble;}
} rnd;

double timeLimit=2.9;
//const int64_t CYCLES_PER_SEC=2800000000;
const int64_t CYCLES_PER_SEC=2400000000;//opt-judge
struct Timer {
	int64_t start;
	Timer() { reset(); }
	void reset() { start = getCycle(); }
	inline double get() { return (double)(getCycle() - start) / CYCLES_PER_SEC; }
	inline int64_t getCycle() {
		uint32_t low, high;
		__asm__ volatile ("rdtsc" : "=a" (low), "=d" (high));
		return ((int64_t)low) | ((int64_t)high << 32);
	}
};
Timer timer;
double LN[65536];
int N,M;
vector<int> g[2000];
uint16_t ans[2000],bcv[2000],cv[2000];
uint8_t indeg[2000];
int bestscore=0;

int init(){
	rep(i,N) cv[i]=i;
	sort(cv,cv+N,[](const int& l,const int& r){
		return indeg[l]<indeg[r];
	});
	rep(i,N) ans[cv[i]]=i;
	int score=0;
	rep(i,N){
		for(auto j:g[i]){
			if(ans[i]>ans[j]) ++score;
		}
	}
	return score;
}

void check(){
	int score=0;
	rep(i,N) ans[bcv[i]]=i;
	rep(i,N){
		for(auto j:g[i]){
			if(ans[i]>ans[j]) ++score;
		}
	}
	cerr<<score<<endl;
}

uint8_t ml[2000][2000];
void improve(){
	double t;
    int score=bestscore;
	int tn=0;
	int diff,mx,nid;
	while(1){
		t=timer.get();
        if(t>timeLimit){
			cerr<<tn<<" "<<bestscore<<endl;
            break;
        }
    	rep(i,N){
			++tn;
			diff=0,mx=-10000,nid=-1;
			int ci=cv[i];
            if(rnd.nextInt()&1){
				FOR(k,i+1,N){
					diff+=ml[cv[k]][ci];
					diff-=ml[ci][cv[k]];
					if(mx<=diff) mx=diff,nid=k;
				}
				diff=0;
				for(int k=i-1;k>=0;--k){
					diff-=ml[cv[k]][ci];
					diff+=ml[ci][cv[k]];
					if(mx<=diff) mx=diff,nid=k;
				}
            }
            else{
                for(int k=i-1;k>=0;--k){
					diff-=ml[cv[k]][ci];
					diff+=ml[ci][cv[k]];
					if(mx<=diff) mx=diff,nid=k;
				}
                diff=0;
                FOR(k,i+1,N){
					diff+=ml[cv[k]][ci];
					diff-=ml[ci][cv[k]];
					if(mx<=diff) mx=diff,nid=k;
				}
            }
			if(mx>=0){
            	score-=mx;
				if(i<nid){
					rotate(cv+i,cv+i+1,cv+nid+1);
				}
				else{
					rotate(cv+nid,cv+i,cv+i+1);
				}
				
				if(bestscore>score){
                	bestscore=score;
					memcpy(bcv,cv,sizeof(cv));
            	}
        	}
			
    	}
	}
}

int main(){
	ios::sync_with_stdio(false),cin.tie(0),cout.tie(0);
	cin>>N>>M;
	int vi,ui;
	rep(i,M){
		cin>>vi>>ui;
		++ml[vi][ui];
		g[vi].pb(ui);
		++indeg[ui];
	}
	int score=init();
	cerr<<score<<endl;
	bestscore=score;

	memcpy(bcv,cv,sizeof(cv));
	improve();

	check();

	rep(i,N) cout<<ans[i]<<"\n";
	cout<<endl;
	return 0;
}