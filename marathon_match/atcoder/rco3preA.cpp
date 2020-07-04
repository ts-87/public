#include <bits/stdc++.h>
#pragma GCC optimize ("-O3")
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

#define eps (1e-6)

double timeLimit=1.98;
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

const int SZ=15;
Timer timer;
int N;
int px[201],py[201];
bool used[201];
double dist[201][201],dist2[201][201];
double ave=0,tot=0,tot2=0,bestscore,besttot;
vector<int> ans,bestans;
int child[201],par[201],bestchild[201],bestpar[201];
vector<int> neigh[201];
set<pint> setX,setY;
//lkh test
void lkh(){
    double curscore,diff,diff2,ctot=tot;
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
        ctot=besttot;
        t0=rnd.nextInt(N);
        stv=t0;
        rep(k,15){

            y1=-1;
            if(k==0)nxx=child[stv];
            //curscore-=dist[stv][nxx];
            if(setY.count({min(nxx,stv),max(nxx,stv)})>0) break;
            setX.insert({min(nxx,stv),max(nxx,stv)});

            //y0=child[nxx];
            //if(y0==t0) break;
            rep(i,15){
                int tmp=neigh[nxx][rnd.nextInt(SZ)];
                diff=ctot+dist[nxx][tmp]-dist[stv][nxx];
                diff2=(curscore+ctot*ctot/N/N)*N+dist2[nxx][tmp]-dist2[stv][nxx];
                y0=par[tmp];
                if(diff2/N-diff*diff/N/N+eps<bestscore
                &&setX.count({min(nxx,tmp),max(nxx,tmp)})==0
                &&setY.count({min(y0,tmp),max(y0,tmp)})==0
                &&child[tmp]!=t0&&y0!=nxx){
                    y1=tmp;
                    break;
                }
            }
            if(y1!=-1){

                setY.insert({min(nxx,y1),max(nxx,y1)});
                curscore=diff2/N-diff*diff/N/N;
                ctot=diff;
                //reverse(nxx,y0);
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

                diff=ctot+dist[y0][t0]-dist[y0][y1];
                diff2=(curscore+ctot*ctot/N/N)*N+dist2[y0][t0]-dist2[y0][y1];
                if(diff2/N-diff*diff/N/N+eps<bestscore){
                    bestscore=diff2/N-diff*diff/N/N;
                    besttot=diff;
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
    timer.reset();
    cin>>N;
    double aa=261;
    rep(i,N) cin>>px[i]>>py[i];
    rep(i,N){
        sorted.clear();
        rep(j,N){
            dist[i][j]=hypot(px[i%N]-px[j%N],py[i%N]-py[j%N]);
            dist2[i][j]=dist[i][j]*dist[i][j];
            if(i!=j) sorted.pb(abs(dist[i][j]-261),j);
        }
        sort(sorted.begin(),sorted.end());
        rep(k,SZ){
            neigh[i].pb(sorted[k].second);
        }
    }
    int cur=0;
    ans.pb(cur);
    used[0]=true;
    while(1){
        double mn=1000100010;
        int nx=-1;
        rep(i,N)if(!used[i]){
            //double tdist=hypot(px[cur]-px[i],py[cur]-py[i]);
            double tdist=abs(dist[cur][i]-aa);
            if(mn>tdist) mn=tdist,nx=i;
        }
        if(nx!=-1){
            child[cur]=nx;
            par[nx]=cur;
            ans.pb(nx),cur=nx,used[nx]=true;
            
            
        }
        else break;
    }
    ans.pb(0);
    child[cur]=0;
    par[0]=cur;
    memcpy(bestchild,child,sizeof(child));
    memcpy(bestpar,par,sizeof(par));

    rep(i,N){
        tot+=dist[ans[i]][ans[i+1]];
        tot2+=dist2[ans[i]][ans[i+1]];
    }
    ave=tot/N;
    bestscore=tot2/N-ave*ave;
    besttot=tot;
    cerr<<ave<<" "<<bestscore<<endl;

    lkh();

    cerr<<bestscore<<endl;
    cur=0;
    rep(i,N){
        cout<<cur<<endl;
        cur=bestchild[cur];
    }
    return 0;
}
