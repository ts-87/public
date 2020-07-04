#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

double timeLimit=2.95,timeLimit2=2.97;
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
Timer timer;
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

double LN[65536];
int N;
int B1,B2,B3;
int L[30][30],R[30][30];
int ans[30][30],bestans[30][30];
int bestscore;
int sx[30],sy[30];

int getScore(){
    int point=0;
    for (int i = 0; i < N; i++)
		{
            int px=0,py=0;
			for (int j = 0; j < N; j++)
			{
				int now = 0;
				for (int k = j; k < N; k++)
				{
					now += ans[k][i];
					if (now == B1) py += B1;
					if (now == B2) py += B2;
					if (now == B3) py += B3;
					if (now > B3) break;
				}
                
                now = 0;
				for (int k = j; k < N; k++)
				{
					now += ans[i][k];
					if (now == B1) px += B1;
					if (now == B2) px += B2;
					if (now == B3) px += B3;
					if (now > B3) break;
				}
			}
            sx[i]=px,sy[i]=py;
            point+=px+py;
		}

        return point;
}
inline pint getScore2(int x,int y){
    int px=0,py=0;
    
    
    rep(i,N){
		int now = 0,nowx=0;
		for (int k = i; k < N; k++){
			now += ans[k][x];
			if (now == B1) py += B1;
			if (now == B2) py += B2;
			if (now == B3) py += B3;
			//if (now > B3) break;

            nowx += ans[y][k];
			if (nowx == B1) px += B1;
			if (nowx == B2) px += B2;
			if (nowx == B3) px += B3;
			if (now > B3&&nowx>B3) break;
		}
    }
    
    return {px,py};
}
void init(){
    rep(i,N)rep(j,N) ans[i][j]=L[i][j];
}
void improve(){
    int turn=0,curscore=bestscore,nxscore,prescore;
    double t,start=timer.get();
	double invtl=1.0/timeLimit,starttemp=30,endtemp=10,ts;
    int cnt=0;
    while(1){
		++turn;
		t=timer.get();
		if(t-start>timeLimit){
			cerr<<turn<<" "<<bestscore<<endl;
            break;
        }
        ts=starttemp+(endtemp-starttemp)*(t)*invtl;
        int x=rnd.nextInt(N),y=rnd.nextInt(N);
        int df=R[y][x]-L[y][x]+1;
        if(df==1) continue;
        int nx=rnd.nextInt(df)+L[y][x],pre=ans[y][x];
        ans[y][x]=nx;
        prescore=sx[y]+sy[x];
        pint pnx=getScore2(x,y);
        int diff=pnx.first+pnx.second-prescore;
        if((diff>=0||diff>LN[rnd.nextInt()&65535]*ts)){
            curscore+=diff;
            sx[y]=pnx.first;sy[x]=pnx.second;
            if(curscore>bestscore){
                bestscore=curscore;
                memcpy(bestans,ans,sizeof(ans));
                //cerr<<bestscore<<endl;
            }
        }
        else{
            ans[y][x]=pre;
        }
    }
}

void improve2(){
    int turn=0,curscore=bestscore,prescore;
    double t,start=timer.get();
    int nxy,nxx,nxn,nxsx,nxsy;
    while(1){
		++turn;
		t=timer.get();
		if(t>timeLimit2){
			cerr<<turn<<" "<<bestscore<<endl;
            break;
        }

        int num=15+(15-10)*(t-start)/(timeLimit2-timeLimit),nxbest=-1000100010;
        if(turn>1){
            ans[nxy][nxx]=nxn;
            sx[nxy]=nxsx;sy[nxx]=nxsy;
        }
		rep(_,num){
            int x=rnd.nextInt(N),y=rnd.nextInt(N);
            int df=R[y][x]-L[y][x]+1;
            //if(df==1) continue;
            int nx=rnd.nextInt(df)+L[y][x],pre=ans[y][x];
            ans[y][x]=nx;
            prescore=sx[y]+sy[x];
            pint pnx=getScore2(x,y);
            int diff=pnx.first+pnx.second-prescore;
            if(nxbest<diff){
                nxbest=diff;
                nxy=y,nxx=x,nxn=nx;
                nxsx=pnx.first;nxsy=pnx.second;
                if(bestscore<curscore+diff){
                    bestscore=curscore+diff;
                    memcpy(bestans,ans,sizeof(ans));
                    //cerr<<turn<<" "<<bestscore<<endl;
                    break;
                }
            }
            
            ans[y][x]=pre;
        }
        curscore+=nxbest;
    }
}
int main(){
    timer.reset();
    cin>>N>>B1>>B2>>B3;
    rep(i,N)rep(j,N) cin>>L[i][j];
    rep(i,N)rep(j,N) cin>>R[i][j];
    double tmpm=1.0/(2.0*65536);
    rep(i,65536){
        LN[i]=log((double)i/65536+tmpm);
        //LN[i]=max(-4.0,LN[i]);
    }
    init();
    memcpy(bestans,ans,sizeof(ans));
    bestscore=getScore();
    cerr<<bestscore<<endl;
    improve();
    memcpy(ans,bestans,sizeof(ans));getScore();
    improve2();
    rep(i,N){
        rep(j,N) cout<<bestans[i][j]<<" ";
        cout<<endl;
    }
    return 0;
}
