#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

double timeLimit=5;//9.97;
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

Timer timer;
int N,M;
vector<int> g[200];
pint deg[200];
int color[200],bestco[200];
ll used[200];
int best,num;
bool dfs(int cur){
    if(timer.get()>timeLimit) return false;
    bool ret=false;
    if(cur==N){
        memcpy(bestco,color,sizeof(color));
        return true;
    }
    int v=deg[cur].second;
    used[v]=0;
    rep(j,g[v].size()){
        if(color[g[v][j]]!=-1) used[v]|=1ll<<color[g[v][j]];
    }
    rep(j,num){
        if((used[v]>>j&1)==0){
            color[v]=j;
            ret|=dfs(cur+1);
            if(ret) return ret;
            color[v]=-1;
        }
    }
    return ret;
}
int numco[30];
int main(){
    timer.reset();
    memset(color,-1,sizeof(color));
    cin>>N>>M;
    int ai,bi;
    rep(i,M){
        cin>>ai>>bi;
        g[ai].pb(bi);
        g[bi].pb(ai);
    }
    rep(i,N){
        deg[i]={g[i].size(),i};
    }
    best=deg[0].first+2;
    sort(deg,deg+N,greater<pint>());
    
    rep(i,N){
        int v=deg[i].second;
        rep(j,g[v].size()){
            if(color[g[v][j]]!=-1) used[v]|=1ll<<color[g[v][j]];
        }
        rep(j,best+1){
            if((used[v]>>j&1)==0){
                color[v]=j;
                break;
            }
        }
    }

    best=*max_element(color,color+N)+1;
    memcpy(bestco,color,sizeof(color));
    vector<int> cand;
    while(1){
        if(timer.get()>timeLimit) break;
        --best;
        queue<int> que;
        rep(i,N)if(color[i]>=best) que.push(i);
        int turn=0;
        while(!que.empty()){
            ++turn;
            if(!(turn&4095)){
                turn=0;
                if(timer.get()>timeLimit) break;
            }
            int tv=que.front();que.pop();
            memset(numco,0,sizeof(numco));
            rep(i,g[tv].size()){
                ++numco[color[g[tv][i]]];
                //numco[color[g[tv][i]]]+=g[g[tv][i]].size();
            }
            int tnum=*min_element(numco,numco+best);
            cand.clear();
            rep(i,best)if(tnum==numco[i]) cand.pb(i);
            int nxco;
            if(rnd.nextInt()&31) nxco=cand[rnd.nextInt(cand.size())];
            else nxco=rnd.nextInt(best);
            color[tv]=nxco;
            rep(i,g[tv].size())if(color[g[tv][i]]==nxco){
                color[g[tv][i]]=best;
                que.push(g[tv][i]);
            }
        }
        if(que.empty()) memcpy(bestco,color,sizeof(color));
        else{
            ++best;
            break;
        }
    }
    
    /*
    num=best;
    while(1){
        memset(color,-1,sizeof(color));
        if(dfs(0)) --num;
        else{
            int l=rnd.nextInt(N),r=rnd.nextInt(N);
            if(l==r) r=(r+1)%N;
            swap(deg[l],deg[r]);
        };
        if(timer.get()>timeLimit) break;
    }
    */
    //rep(i,N) cout<<color[i]<<endl;
    
    rep(i,N) cout<<bestco[i]<<endl;
    return 0;
}