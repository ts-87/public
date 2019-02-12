#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

#define eps (1e-6)

double timeLimit=9.97;
const int64_t CYCLES_PER_SEC=2800000000;
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

const int SZ=20;
Timer timer;
int N;
int px[1001],py[1001],child[1001],bestchild[1001],par[1001],bestpar[1001];
vector<int> neigh[1001];
bool used[1001];
double bestscore;
//N<=1000 small N
double dist[1001][1001];

set<pint> setX,setY;
void lkh(){
    double curscore,diff;
    double t;
    int t0,stv,nxx,y0,y1;
    while(1){
        t=timer.get();
        if(t>timeLimit){
            return;
        }
        memcpy(child,bestchild,sizeof(child));
        memcpy(par,bestpar,sizeof(par));
        setX.clear(),setY.clear();
        curscore=bestscore;
        t0=rnd.nextInt(N);
        stv=t0;
        rep(k,15){

            y1=-1;
            if(k==0)nxx=child[stv];
            if(setY.count({min(nxx,stv),max(nxx,stv)})>0) break;
            setX.insert({min(nxx,stv),max(nxx,stv)});

            rep(i,15){
                int tmp=neigh[nxx][rnd.nextInt(SZ)];
                diff=dist[nxx][tmp]-dist[stv][nxx];
                y0=par[tmp];
                if(curscore+diff+eps<bestscore
                &&setX.count({min(nxx,tmp),max(nxx,tmp)})==0
                &&setY.count({min(y0,tmp),max(y0,tmp)})==0
                &&child[tmp]!=t0&&y0!=nxx){
                    y1=tmp;
                    break;
                }
            }

            if(y1!=-1){

                setY.insert({min(nxx,y1),max(nxx,y1)});
                curscore+=diff;
                int cur=nxx,ch=child[nxx],chch;
                while(1){
                    chch=child[ch];
                    child[ch]=cur;
                    par[cur]=ch;
                    cur=ch;
                    ch=chch;
                    if(cur==y0) break;
                }
                child[nxx]=y1;
                par[y1]=nxx;
                if(curscore+dist[y0][t0]-dist[y0][y1]+eps<bestscore){
                    bestscore=curscore+dist[y0][t0]-dist[y0][y1];
                    memcpy(bestchild,child,sizeof(child));
                    memcpy(bestpar,par,sizeof(par));
                    bestchild[t0]=y0;
                    bestpar[y0]=t0;
                    break;
                }

                
                stv=y1;
                nxx=y0;
            }
            else break;
        }
    }
    return;
}
vector<pair<double,int> > sorted;
int main(){
    cin>>N;
    rep(i,N) cin>>px[i]>>py[i];
    timer.reset();
    rep(i,N){
        sorted.clear();
        rep(j,N){
            dist[i][j]=hypot(px[i%N]-px[j%N],py[i%N]-py[j%N]);
            if(i!=j)sorted.pb(dist[i][j],j);
        }
        sort(sorted.begin(),sorted.end());
        rep(k,SZ){
            neigh[i].pb(sorted[k].second);
        }
    }
    double curscore=0;
    int cur=0;
    used[0]=true;
    while(1){
        double mn=1000100010;
        int nx=-1;
        rep(i,N)if(!used[i]){
            //double tdist=hypot(px[cur]-px[i],py[cur]-py[i]);
            double tdist=dist[cur][i];
            if(mn>tdist) mn=tdist,nx=i;
        }
        if(nx!=-1){
            child[cur]=nx;
            par[nx]=cur;
            cur=nx,curscore+=mn,used[nx]=true;
        }
        else break;
    }
    curscore+=dist[cur][0];
    child[cur]=0;
    par[0]=cur;
    bestscore=curscore;
    memcpy(bestchild,child,sizeof(child));
    memcpy(bestpar,par,sizeof(par));

    lkh();
    //0-index
    cerr<<bestscore<<endl;
    cur=0;
    rep(i,N){
        cout<<cur<<endl;
        cur=bestchild[cur];
    }
    return 0;
}