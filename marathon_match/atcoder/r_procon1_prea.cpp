#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

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

double timeLimit=1.97;
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
} timer;
double LN[65536];
int N,W,K,V,H;
int g[127][8],bestg[127][8],rvg[8][127];
int rowscore[128],rowcnt[2][8],rrcnt[127][8];
pint info[1001];
int cnt[127][8];
int bestscore=0;
int ans[1001];
vector<int> cand;
void improve(){
    double t,start=timer.get(),ts,starttemp=2.5,endtemp=0.001,invtl=1.0/(timeLimit-start);
    int curscore=bestscore;
    int turn=0;
    int lw,rw,lh,rh,rhl,rhh;
    while(1){
        t=timer.get();
        if(t>timeLimit){
            cerr<<turn<<" "<<bestscore<<endl;
            break;
        }
        ts=starttemp+(endtemp-starttemp)*(t-start)*invtl;
        for(lh=1;lh<=H;++lh)for(lw=0;lw<W;++lw)for(rw=0;rw<W;++rw)if(lw!=rw){
        rhh=lower_bound(rvg[rw],rvg[rw]+127,g[lh][lw])-rvg[rw];
        rhl=min(H,rhh);
        cand.clear();
        while(rhl&&g[lh][lw]<rvg[rw][rhl+1]&&g[lh-1][lw]<rvg[rw][rhl]){
            if(g[lh+1][lw]>rvg[rw][rhl]){
                cand.pb(rhl);
            }
            --rhl;
        }
        if(cand.size()==0) continue;
        rh=cand[rnd.nextInt(cand.size())];
        ++turn;
        if(lh==rh){
            swap(g[lh][lw],g[rh][rw]);
            swap(rvg[lw][lh],rvg[rw][rh]);
            continue;
        }
        int mx1=0,mx2=0;
        pint ifr=info[g[rh][rw]],ifl=info[g[lh][lw]]; 
        rrcnt[lh][ifr.first]+=ifr.second;
        rrcnt[lh][ifl.first]-=ifl.second;
        rrcnt[rh][ifl.first]+=ifl.second;
        rrcnt[rh][ifr.first]-=ifr.second;
        
        rep(i,K){
            if(mx1<rrcnt[lh][i]) mx1=rrcnt[lh][i];
            if(mx2<rrcnt[rh][i]) mx2=rrcnt[rh][i];
        }
        
        int diff=mx1+mx2-rowscore[lh]-rowscore[rh];
		if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
            curscore+=diff;
            rowscore[lh]=mx1;rowscore[rh]=mx2;
            swap(g[lh][lw],g[rh][rw]);
            swap(rvg[lw][lh],rvg[rw][rh]);
            if(bestscore<curscore){
                bestscore=curscore;
                memcpy(bestg,g,sizeof(bestg));
            }
        }
        else{
            rrcnt[lh][ifr.first]-=ifr.second;
            rrcnt[lh][ifl.first]+=ifl.second;
            rrcnt[rh][ifl.first]-=ifl.second;
            rrcnt[rh][ifr.first]+=ifr.second;
        }
        }
    }
}
int curh[7],curw[128];
int main(){
    timer.reset();
    double tmpm=1.0/(2.0*65536);
    rep(i,65536){
        LN[i]=log((double)i/65536+tmpm);
    }
    cin>>N>>W>>K>>V;
    H=N/W;
    rep(i,W){
        g[0][i]=-1;
        g[126][i]=N;
        rvg[i][0]=-1;
        rvg[i][126]=N;
    }
    int ci,vi;
    rep(i,N){
        cin>>ci>>vi;
        info[i]={ci,vi};
    }
    int score=0;
    
    int lst=0;
    rep(i,N){
        int mx=-1,nw,nh;
        rep(j,W){
            if(curh[j]<H&&curh[j]-lst<50&&cnt[curh[j]+1][info[i].first]+info[i].second>mx){
                mx=cnt[curh[j]+1][info[i].first]+info[i].second;
                nw=j;nh=curh[j]+1;
            }
        }
        ++curw[nh];
        if(curw[nh]==W) ++lst;
        ++curh[nw];
        cnt[nh][info[i].first]+=info[i].second;
        g[nh][nw]=i;
        rvg[nw][nh]=i;
    }
    /*
    rep(i,N){
        g[i/W+1][i%W]=i;
        rvg[i%W][i/W+1]=i;
        cnt[i/W+1][info[i].first]+=info[i].second;
    }
    */
    rep(i,H){
        int mx=0;
        rep(j,K)if(mx<cnt[i+1][j]){
            mx=cnt[i+1][j];
        }
        rowscore[i+1]=mx;
        score+=mx;
    }
    memcpy(rrcnt,cnt,sizeof(cnt));
    bestscore=score;
    memcpy(bestg,g,sizeof(bestg));
    cerr<<score<<endl;
    improve();
    rep(i,N){
        ans[bestg[i/W+1][i%W]]=i%W;
    }
    rep(i,N) cout<<ans[i]<<endl;
    return 0;
}