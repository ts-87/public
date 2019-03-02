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
	void setseed(uint32_t seed){z^=seed;}
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
};
Timer timer;
int N,M;
int C[21][21],tC[21][21],initC[21][21];
int bestscore=0,score=0;
vector<int> AR,AC,AS,bAR,bAC,bAS;
pint getScore(){
    int ret=0,ret2=0;
    rep(i,N)rep(j,N){
        if(tC[i][j]==0){
            if(i<10&&j<10) ++ret;
            ret2+=N+N-i-j;
        }
        else if(tC[i][j]==1){
            if(i<10&&j>=10) ++ret;
            ret2+=N-i+j;
        }
        else if(tC[i][j]==2){
            if(i>=10&&j<10) ++ret;
            ret2+=N+i-j;
        }
        else if(tC[i][j]==3){
            if(i>=10&&j>=10) ++ret;
            ret2+=i+j;
        }
    }
    return {ret,ret2};
}
inline bool forceupdate(double diff,double temp){
    if(diff>=0) return true;
    double d=(diff)*temp;
    if(d<-6) return false;
    return exp(d)>rnd.nextDouble();
}
void improve(){
    double start=timer.get(),curtemp;
    int turn=0;
    double t=start,invtl=1.0/timeLimit;
    memcpy(C,initC,sizeof(C));
    
    int rot=rnd.nextInt(4);
    rep(i,rot){
        int tr=0,tc=0,ts=N;
        rep(i,ts/2)rep(j,(ts+1)/2){
            int fst=C[tr+i][tc+j];
            C[tr+i][tc+j]=C[tr+ts-1-j][tc+i];
            C[tr+ts-1-j][tc+i]=C[tr+ts-1-i][tc+ts-1-j];
            C[tr+ts-1-i][tc+ts-1-j]=C[tr+j][tc+ts-1-i];
            C[tr+j][tc+ts-1-i]=fst;
        }
        AR.emplace_back(0);
        AC.emplace_back(0);
        AS.emplace_back(N);
    }
    
    memcpy(tC,C,sizeof(tC));
    pint tmp=getScore();
    int curscore=tmp.first;
    int vscore=tmp.second;
    int bestv=vscore;
    bestscore=curscore;
    double starttemp=1,endtemp=0.01;
    while(1){
        ++turn;
        t=timer.get();
        if(t-start>timeLimit/50){
            cerr<<turn<<endl;
            return;
        }
        curtemp=1.0/(starttemp+(endtemp-starttemp)*t*invtl);
        int ts=rnd.nextInt(N-1)+2;
        int tr,tc;
        tr=rnd.nextInt(N-ts+1);
        tc=rnd.nextInt(N-ts+1);
        int sel=rnd.nextInt()&1;
        memcpy(tC,C,sizeof(tC));
        /*
        if(sel==0&&3+AR.size()<=M){
            rep(i,ts/2)rep(j,(ts+1)/2){
                int fst=tC[tr+i][tc+j];
                tC[tr+i][tc+j]=tC[tr+j][tc+ts-1-i];
                tC[tr+j][tc+ts-1-i]=tC[tr+ts-1-i][tc+ts-1-j];
                tC[tr+ts-1-i][tc+ts-1-j]=tC[tr+ts-1-j][tc+i];
                tC[tr+ts-1-j][tc+i]=fst;
            }
        }
        */
        if(sel==1&&2+AR.size()<=M)rep(i,ts/2)rep(j,(ts+1)/2){
            int fst=tC[tr+i][tc+j];
            tC[tr+i][tc+j]=tC[tr+ts-1-j][tc+i];
            tC[tr+ts-1-j][tc+i]=tC[tr+ts-1-i][tc+ts-1-j];
            tC[tr+ts-1-i][tc+ts-1-j]=tC[tr+j][tc+ts-1-i];
            tC[tr+j][tc+ts-1-i]=fst;
            fst=tC[tr+i][tc+j];
            tC[tr+i][tc+j]=tC[tr+ts-1-j][tc+i];
            tC[tr+ts-1-j][tc+i]=tC[tr+ts-1-i][tc+ts-1-j];
            tC[tr+ts-1-i][tc+ts-1-j]=tC[tr+j][tc+ts-1-i];
            tC[tr+j][tc+ts-1-i]=fst;
        }
        
        else{
            rep(i,ts/2)rep(j,(ts+1)/2){
                int fst=tC[tr+i][tc+j];
                tC[tr+i][tc+j]=tC[tr+ts-1-j][tc+i];
                tC[tr+ts-1-j][tc+i]=tC[tr+ts-1-i][tc+ts-1-j];
                tC[tr+ts-1-i][tc+ts-1-j]=tC[tr+j][tc+ts-1-i];
                tC[tr+j][tc+ts-1-i]=fst;
            }
        }
        
        pint tmp=getScore();
        int curscore=tmp.first;
        int vscore=tmp.second;
        if(vscore>bestv){
            bestv=vscore;
            if(bestscore<curscore) bestscore=curscore;
            /*
            if(sel==0&&3+AR.size()<=M){
                AR.emplace_back(tr);
                AC.emplace_back(tc);
                AS.emplace_back(ts);
                AR.emplace_back(tr);
                AC.emplace_back(tc);
                AS.emplace_back(ts);
                AR.emplace_back(tr);
                AC.emplace_back(tc);
                AS.emplace_back(ts);
            }
            */
            if(sel==1&&2+AR.size()<=M){
                AR.emplace_back(tr);
                AC.emplace_back(tc);
                AS.emplace_back(ts);
                AR.emplace_back(tr);
                AC.emplace_back(tc);
                AS.emplace_back(ts);
            }
            
            else{
                AR.emplace_back(tr);
                AC.emplace_back(tc);
                AS.emplace_back(ts);
            }
            memcpy(C,tC,sizeof(C));
            if(bestscore==N*N||AR.size()==M) return;
        }
    }
}
int main(){
    timer.reset();
    cin>>N>>M;
    rep(i,N)rep(j,N) cin>>C[i][j];
    int fscore=0;
    memcpy(initC,C,sizeof(initC));
    rep(i,50){
        improve();
        if(fscore<bestscore){
            fscore=bestscore;
            bAR=AR;bAC=AC;bAS=AS;
        }
        cerr<<bestscore<<endl;
        AR.clear();AC.clear();AS.clear();
    }
    cerr<<fscore<<endl;
    rep(i,bAR.size()){
        cout<<bAR[i]<<" "<<bAC[i]<<" "<<bAS[i]<<endl;
    }
    
    return 0;
}
