#include <bits/stdc++.h>
#pragma GCC optimize ("-O3")
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

double timeLimit=900,timeLimit2=20,timeLimit3=20;
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
bool ANTI_OF=true;
struct Rand {
	uint32_t x,y,z,w;
	static constexpr double iDouble=1.0/(1LL<<32);
	Rand(uint32_t w):w(w){
		x=123456789;y=362436069;z=521288629;
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
} rnd(ANTI_OF?random_device()():88675123);

const int N=197769;

const int max_n=N+1;
bool is_prime[max_n];
int sieve(int n){
    int p=0;
    rep(i,n+1) is_prime[i]=true;
    is_prime[0]=is_prime[1]=false;
    FOR(i,2,n+1){
        if(is_prime[i]){
            for(int j=2*i;j<=n;j+=i) is_prime[j]=false;
        }
    }
    return p;
}

double X[N],Y[N];
bool used[N];
int ans[N+1],bestans[N+1];
double bestscore;
Timer timer;

//const int RX=10,RY=10,uX=5100/10,uY=3400/10,numgrid=RX*RY;
const int RX=17,RY=17,uX=5100/17,uY=3400/17,numgrid=RX*RY;
//const int RX=17,RY=10,uX=5100/17,uY=3400/10,numgrid=RX*RY;
//const int RX=10,RY=17,uX=5100/10,uY=3400/17,numgrid=RX*RY;
//const int RX=17,RY=34,uX=5100/17,uY=3400/34,numgrid=RX*RY;

//const int RX=5,RY=17,uX=5100/5,uY=3400/17,numgrid=RX*RY;

vector<int> rgv[RX*RY];
double sqdence[numgrid+1];
double init(){
    double ret=0;
    used[0]=true;
    int cur=0;
    ans[0]=0;
    FOR(k,1,N){
        double mn=1000100010;
        int nx=-1;
        rep(i,N)if(!used[i]){
            double dist=hypot(X[cur]-X[i],Y[cur]-Y[i]);
            if(mn>dist) mn=dist,nx=i;
        }
        ans[k]=nx,cur=nx,ret+=mn,used[nx]=true;
    }
    ans[N]=0;
    ret+=hypot(X[cur]-X[0],Y[cur]-Y[0]);
    return ret;
}

double init1_2(){
    double ret=0;
    used[0]=true;
    int cur=0;
    ans[0]=0;
    FOR(k,1,N){
        double mn=1000100010,mn2=mn;
        int nx=-1;
        rep(i,N)if(!used[i]){
            double dist=hypot(X[cur]-X[i],Y[cur]-Y[i]);
            double dist2=hypot(X[cur]-X[i],(Y[cur]-Y[i]));
            if(mn2>dist2) mn=dist,nx=i,mn2=dist2;
        }
        ans[k]=nx,cur=nx,ret+=mn,used[nx]=true;
    }
    ans[N]=0;
    ret+=hypot(X[cur]-X[0],Y[cur]-Y[0]);
    return ret;
}

int neigh[N][25];
const int neiSZ=15;
double init2(){
    sieve(N);
    double ret;
    //ifstream ifs("./bestans_noprime.txt");
    ifstream ifs("./bestans2.txt");
    if(ifs.fail()) exit(0);
    ifs>>ret;
    rep(i,N+1) ifs>>ans[i];
    ifstream ifs2("./neighbor.txt");
    if(ifs2.fail()) exit(0);
    rep(i,N){
        rep(j,25) ifs2>>neigh[i][j];
    }
    ret=0;
    double dist;
    rep(i,N){
        dist=hypot(X[ans[i+1]]-X[ans[i]],Y[ans[i+1]]-Y[ans[i]]);
        if(i%10==9&&!is_prime[ans[i]]) dist*=1.1;
        ret+=dist;
    }
    cerr<<setprecision(12)<<fixed<<ret<<endl;
    cerr<<"ifs_ok"<<endl;
    return ret;
}

inline bool forceupdate(double sub,double temp){
    if(sub>=0) return true;
    double d=(sub)*temp;
    //if(d<-6) return false;
    return exp(d)>rnd.nextDouble();
}

int perm[N];

void improve1(int tg){
    double start=timer.get();
    double curscore=bestscore;
    double pre,nx,dist;
    double starttemp,endtemp=0.001,curtemp;
    double limit;
    if(tg!=numgrid)starttemp=max(0.05/sqdence[tg],0.05),limit=timeLimit2;
    //if(tg!=numgrid)starttemp=max(0.05/sqdence[tg],0.05),limit=timeLimit2;
    else starttemp=0.05,limit=timeLimit3;

    ll turn=0;
    double t,invtl=1.0/limit;
    while(1){
        ++turn;
        
        t=timer.get();
        if(t-start>limit){
            cerr<<turn<<" "<<curscore<<endl;
            //cerr<<turn<<endl;
            return;
        }

        curtemp=1.0/(starttemp+(endtemp-starttemp)*(t-start)*invtl);
        

        int id1,id2,rsz=rgv[tg].size();
        if(tg!=numgrid)while(1){
            id1=perm[rgv[tg][rnd.nextInt(rsz)]];
            id2=perm[rgv[tg][rnd.nextInt(rsz)]];
            if(id1!=0&&id2!=0&&id1!=id2) break;
        }
        
        else{
            while(1){
                id1=rnd.nextInt(N-1)+1,id2=rnd.nextInt(N-1)+1;
                int sub=abs(id1-id2);
                if(sub>0) break;
            }
        }

        if(id1>id2) swap(id1,id2);

        double nxscore=curscore;

        pre=0,nx=0;
        
        pre+=hypot(X[ans[id1]]-X[ans[id1-1]],Y[ans[id1]]-Y[ans[id1-1]]);
        nx+=hypot(X[ans[id2]]-X[ans[id1-1]],Y[ans[id2]]-Y[ans[id1-1]]);
        if(id1%10==0&&!is_prime[ans[id1-1]]) pre*=1.1,nx*=1.1;

        dist=hypot(X[ans[id2+1]]-X[ans[id2]],Y[ans[id2+1]]-Y[ans[id2]]);
        if((id2+1)%10==0&&!is_prime[ans[id2]]) dist*=1.1;
        pre+=dist;
        dist=hypot(X[ans[id2+1]]-X[ans[id1]],Y[ans[id2+1]]-Y[ans[id1]]);
        if((id2+1)%10==0&&!is_prime[ans[id1]]) dist*=1.1;
        nx+=dist;

        for(int i=9-id1%10;i<id2-id1;i+=10){
            if(!is_prime[ans[id1+i]]){
                dist=hypot(X[ans[id1+1+i]]-X[ans[id1+i]],Y[ans[id1+1+i]]-Y[ans[id1+i]]);
                dist*=0.1,pre+=dist;
            }
            
            if(!is_prime[ans[id2-i]]){
                dist=hypot(X[ans[id2-i]]-X[ans[id2-i-1]],Y[ans[id2-i]]-Y[ans[id2-i-1]]);
                dist*=0.1,nx+=dist;
            }
            
        }
        nxscore+=nx-pre;

        //if(turn<3||curscore>=nxscore){
        if(forceupdate(curscore-nxscore,curtemp)){
            curscore=nxscore;
            //reverse(ans+(id1),ans+(id2+1));
            rep(i,(id2-id1+1)/2){
                swap(ans[id2-i],ans[id1+i]);
                swap(perm[ans[id2-i]],perm[ans[id1+i]]);
            }
            if(bestscore>nxscore){
                bestscore=nxscore;
                memcpy(bestans,ans,sizeof(ans));

                //if(turn%20==0)cerr<<setprecision(12)<<fixed<<bestscore<<endl;
                cerr<<setprecision(12)<<fixed<<bestscore<<endl;

                
            }
            /*
            double check=0;
                
            rep(i,N){
                dist=hypot(X[ans[i+1]]-X[ans[i]],Y[ans[i+1]]-Y[ans[i]]);
                if(i%10==9&&!is_prime[ans[i]]) dist*=1.1;
                check+=dist;
            }
            cerr<<setprecision(12)<<fixed<<curscore<<" "<<check<<endl;
            */
        }
    }
}

void improve2(int tg){
    double start=timer.get();
    double curscore=bestscore;
    double pre,nx,dist;
    double starttemp,endtemp=0.01,curtemp;
    double limit;
    //if(tg!=numgrid)starttemp=max(0.2/sqdence[tg],0.05),limit=timeLimit2;
    if(tg!=numgrid)starttemp=max(0.06/sqdence[tg],0.06),limit=timeLimit2;
    else starttemp=0.05,limit=timeLimit3;

    ll turn=0;
    double t,invtl=1.0/limit;
    while(1){
        ++turn;
        
        
        t=timer.get();
        if(t-start>limit) return;
        //curtemp=1.0/(starttemp+(endtemp-starttemp)*(t-start)*invtl);
        

        int id1,id2,rsz=rgv[tg].size();
        if(tg!=numgrid)while(1){
            id1=perm[rgv[tg][rnd.nextInt(rsz)]];
            id2=perm[rgv[tg][rnd.nextInt(rsz)]];
            if(id1!=0&&id2!=0&&id1!=id2) break;
        }
        
        else{
            while(1){
                id1=rnd.nextInt(N-1)+1,id2=rnd.nextInt(N-1)+1;
                int sub=abs(id1-id2);
                if(sub>0) break;
            }
        }


        double nxscore=curscore;

        pre=0,nx=0;

        if(id1<id2){
            dist=hypot(X[ans[id1]]-X[ans[id1-1]],Y[ans[id1]]-Y[ans[id1-1]]);
            if((id1)%10==0&&!is_prime[ans[id1-1]]) dist*=1.1;
            pre+=dist;
            dist=hypot(X[ans[id1]]-X[ans[id1+1]],Y[ans[id1]]-Y[ans[id1+1]]);
            if((id1+1)%10==0&&!is_prime[ans[id1]]) dist*=1.1;
            pre+=dist;
            dist=hypot(X[ans[id2]]-X[ans[id2+1]],Y[ans[id2]]-Y[ans[id2+1]]);
            if((id2+1)%10==0&&!is_prime[ans[id2]]) dist*=1.1;
            pre+=dist;
            dist=hypot(X[ans[id1+1]]-X[ans[id1-1]],Y[ans[id1+1]]-Y[ans[id1-1]]);
            if((id1)%10==0&&!is_prime[ans[id1-1]]) dist*=1.1;
            nx+=dist;
            dist=hypot(X[ans[id1]]-X[ans[id2+1]],Y[ans[id1]]-Y[ans[id2+1]]);
            if((id2+1)%10==0&&!is_prime[ans[id1]]) dist*=1.1;
            nx+=dist;
            dist=hypot(X[ans[id1]]-X[ans[id2]],Y[ans[id1]]-Y[ans[id2]]);
            if((id2)%10==0&&!is_prime[ans[id2]]) dist*=1.1;
            nx+=dist;

            for(int i=9-id1%10;i<id2-id1;i+=10){
                if(i!=0&&!is_prime[ans[id1+i]]){
                    dist=hypot(X[ans[id1+1+i]]-X[ans[id1+i]],Y[ans[id1+1+i]]-Y[ans[id1+i]]);
                    dist*=0.1,pre+=dist;
                }
                if(i!=id2-id1-1&&!is_prime[ans[id1+i+1]]){
                    dist=hypot(X[ans[id1+i+2]]-X[ans[id1+i+1]],Y[ans[id1+i+2]]-Y[ans[id1+i+1]]);
                    dist*=0.1,nx+=dist;
                }
            
            }

        }
        else{
            dist=hypot(X[ans[id1]]-X[ans[id1-1]],Y[ans[id1]]-Y[ans[id1-1]]);
            if((id1)%10==0&&!is_prime[ans[id1-1]]) dist*=1.1;
            pre+=dist;
            dist=hypot(X[ans[id1]]-X[ans[id1+1]],Y[ans[id1]]-Y[ans[id1+1]]);
            if((id1+1)%10==0&&!is_prime[ans[id1]]) dist*=1.1;
            pre+=dist;
            dist=hypot(X[ans[id2]]-X[ans[id2-1]],Y[ans[id2]]-Y[ans[id2-1]]);
            if((id2)%10==0&&!is_prime[ans[id2-1]]) dist*=1.1;
            pre+=dist;
            dist=hypot(X[ans[id1+1]]-X[ans[id1-1]],Y[ans[id1+1]]-Y[ans[id1-1]]);
            if((id1+1)%10==0&&!is_prime[ans[id1-1]]) dist*=1.1;
            nx+=dist;
            dist=hypot(X[ans[id1]]-X[ans[id2]],Y[ans[id1]]-Y[ans[id2]]);
            if((id2+1)%10==0&&!is_prime[ans[id1]]) dist*=1.1;
            nx+=dist;
            dist=hypot(X[ans[id1]]-X[ans[id2-1]],Y[ans[id1]]-Y[ans[id2-1]]);
            if((id2)%10==0&&!is_prime[ans[id2-1]]) dist*=1.1;
            nx+=dist;

            for(int i=9-id2%10;i<id1-id2;i+=10){
                if(i!=id1-id2-1){
                    dist=hypot(X[ans[id2+i+1]]-X[ans[id2+i]],Y[ans[id2+i+1]]-Y[ans[id2+i]]);
                    if(!is_prime[ans[id2+i]]) dist*=0.1,pre+=dist;
                }
                if(i!=0){
                    dist=hypot(X[ans[id2+i]]-X[ans[id2+i-1]],Y[ans[id2+i]]-Y[ans[id2+i-1]]);
                    if(!is_prime[ans[id2+i-1]]) dist*=0.1,nx+=dist;
                }
            
            }

        }


        nxscore+=nx-pre;

        if(curscore>=nxscore){
        //if(forceupdate(curscore-nxscore,curtemp)){
            curscore=nxscore;
            if(id1<id2){
                FOR(i,id1,id2){
                    swap(ans[i],ans[i+1]);
                    --perm[ans[i]];
                }
                perm[ans[id1]]=id2;
            }
            else{
                for(int i=id1;i>id2;--i){
                    swap(ans[i],ans[i-1]);
                    ++perm[ans[i]];
                }
                perm[ans[id1]]=id2;
            }
            if(bestscore>nxscore){
                bestscore=nxscore;
                memcpy(bestans,ans,sizeof(ans));

                //if(turn%20==0)cerr<<setprecision(12)<<fixed<<bestscore<<endl;
                cerr<<setprecision(12)<<fixed<<bestscore<<endl;

                turn=0;

            }
            /*
            double check=0;
                
            rep(i,N){
                dist=hypot(X[ans[i+1]]-X[ans[i]],Y[ans[i+1]]-Y[ans[i]]);
                if(i%10==9&&!is_prime[ans[i]]) dist*=1.1;
                check+=dist;
            }
            cerr<<setprecision(12)<<fixed<<curscore<<" "<<check<<endl;
            */
        }
    }
}
void improve3(int tg){
    double start=timer.get();
    double curscore=bestscore;
    double pre,nx,dist;
    double starttemp,endtemp=0.01,curtemp;
    double limit=timeLimit2;
    if(tg!=numgrid)starttemp=min(0.05/sqdence[tg],0.5),limit=timeLimit2;
    //if(tg!=numgrid)starttemp=min(0.3/sqdence[tg],5.0),limit=timeLimit2;
    //if(tg!=numgrid)starttemp=1.0,limit=timeLimit2;
    else starttemp=0.05,limit=timeLimit3;

    ll turn=0;
    ll cnt=0;
    double t,invtl=1.0/limit;
    while(1){
        ++turn;
        
        
        t=timer.get();
        if(t-start>limit){
            cerr<<turn<<" "<<curscore<<endl;
            return;
        }
        curtemp=1.0/(starttemp+(endtemp-starttemp)*(t-start)*invtl);

        //if(cnt<5) curtemp*=0.3;


        int tid,id1,id2,rsz=rgv[tg].size();
        /*
        if(tg!=numgrid)while(1){
            id1=perm[rgv[tg][rnd.nextInt(rsz)]];
            id2=perm[rgv[tg][rnd.nextInt(rsz)]];
            if(id1!=0&&id2!=0&&id1!=id2) break;
        }
        else{
            while(1){
                id1=rnd.nextInt(N-1)+1,id2=rnd.nextInt(N-1)+1;
                int sub=abs(id1-id2);
                if(sub>0) break;
            }
        }
        */
        if(tg!=numgrid)while(1){
            tid=rgv[tg][rnd.nextInt(rsz)];
            id1=perm[tid];
            id2=perm[neigh[tid][rnd.nextInt(neiSZ)]];
            if(id1!=0&&id2!=0&&id1!=id2) break;
        }
        else{
            while(1){
                tid=rnd.nextInt(N-1)+1;
                id1=perm[tid];
                id2=perm[neigh[tid][rnd.nextInt(neiSZ)]];
                int sub=abs(id1-id2);
                if(sub>0&&id2!=0) break;
            }
        }

        double nxscore=curscore;
        pre=0,nx=0;

        int sel=rnd.nextInt()&1;
        if(sel){
            if(id1>id2) swap(id1,id2);
        
            pre+=hypot(X[ans[id1]]-X[ans[id1-1]],Y[ans[id1]]-Y[ans[id1-1]]);
            nx+=hypot(X[ans[id2]]-X[ans[id1-1]],Y[ans[id2]]-Y[ans[id1-1]]);
            if(id1%10==0&&!is_prime[ans[id1-1]]) pre*=1.1,nx*=1.1;

            dist=hypot(X[ans[id2+1]]-X[ans[id2]],Y[ans[id2+1]]-Y[ans[id2]]);
            if((id2+1)%10==0&&!is_prime[ans[id2]]) dist*=1.1;
            pre+=dist;
            dist=hypot(X[ans[id2+1]]-X[ans[id1]],Y[ans[id2+1]]-Y[ans[id1]]);
            if((id2+1)%10==0&&!is_prime[ans[id1]]) dist*=1.1;
            nx+=dist;

            for(int i=9-id1%10;i<id2-id1;i+=10){
                dist=hypot(X[ans[id1+1+i]]-X[ans[id1+i]],Y[ans[id1+1+i]]-Y[ans[id1+i]]);
                if(!is_prime[ans[id1+i]]) dist*=0.1,pre+=dist;;
            
                dist=hypot(X[ans[id2-i]]-X[ans[id2-i-1]],Y[ans[id2-i]]-Y[ans[id2-i-1]]);
                if(!is_prime[ans[id2-i]]) dist*=0.1,nx+=dist;;
            
            }
            nxscore+=nx-pre;

            //if(curscore>=nxscore){
            if(forceupdate(curscore-nxscore,curtemp)){
                curscore=nxscore;
                rep(i,(id2-id1+1)/2){
                    swap(ans[id2-i],ans[id1+i]);
                    swap(perm[ans[id2-i]],perm[ans[id1+i]]);
                }
                if(bestscore>nxscore){
                    bestscore=nxscore;
                    memcpy(bestans,ans,sizeof(ans));

                    cerr<<setprecision(12)<<fixed<<bestscore<<endl;
                }
                ++cnt;
            }
        }
        else{
            if(id1<id2){
                dist=hypot(X[ans[id1]]-X[ans[id1-1]],Y[ans[id1]]-Y[ans[id1-1]]);
                if((id1)%10==0&&!is_prime[ans[id1-1]]) dist*=1.1;
                pre+=dist;
                dist=hypot(X[ans[id1]]-X[ans[id1+1]],Y[ans[id1]]-Y[ans[id1+1]]);
                if((id1+1)%10==0&&!is_prime[ans[id1]]) dist*=1.1;
                pre+=dist;
                dist=hypot(X[ans[id2]]-X[ans[id2+1]],Y[ans[id2]]-Y[ans[id2+1]]);
                if((id2+1)%10==0&&!is_prime[ans[id2]]) dist*=1.1;
                pre+=dist;
                dist=hypot(X[ans[id1+1]]-X[ans[id1-1]],Y[ans[id1+1]]-Y[ans[id1-1]]);
                if((id1)%10==0&&!is_prime[ans[id1-1]]) dist*=1.1;
                nx+=dist;
                dist=hypot(X[ans[id1]]-X[ans[id2+1]],Y[ans[id1]]-Y[ans[id2+1]]);
                if((id2+1)%10==0&&!is_prime[ans[id1]]) dist*=1.1;
                nx+=dist;
                dist=hypot(X[ans[id1]]-X[ans[id2]],Y[ans[id1]]-Y[ans[id2]]);
                if((id2)%10==0&&!is_prime[ans[id2]]) dist*=1.1;
                nx+=dist;

                for(int i=9-id1%10;i<id2-id1;i+=10){
                    if(i!=0){
                        dist=hypot(X[ans[id1+1+i]]-X[ans[id1+i]],Y[ans[id1+1+i]]-Y[ans[id1+i]]);
                        if(!is_prime[ans[id1+i]]) dist*=0.1,pre+=dist;
                    }
                    if(i!=id2-id1-1){
                        dist=hypot(X[ans[id1+i+2]]-X[ans[id1+i+1]],Y[ans[id1+i+2]]-Y[ans[id1+i+1]]);
                        if(!is_prime[ans[id1+i+1]]) dist*=0.1,nx+=dist;
                    }
            
                }

            }
            else{
                dist=hypot(X[ans[id1]]-X[ans[id1-1]],Y[ans[id1]]-Y[ans[id1-1]]);
                if((id1)%10==0&&!is_prime[ans[id1-1]]) dist*=1.1;
                pre+=dist;
                dist=hypot(X[ans[id1]]-X[ans[id1+1]],Y[ans[id1]]-Y[ans[id1+1]]);
                if((id1+1)%10==0&&!is_prime[ans[id1]]) dist*=1.1;
                pre+=dist;
                dist=hypot(X[ans[id2]]-X[ans[id2-1]],Y[ans[id2]]-Y[ans[id2-1]]);
                if((id2)%10==0&&!is_prime[ans[id2-1]]) dist*=1.1;
                pre+=dist;
                dist=hypot(X[ans[id1+1]]-X[ans[id1-1]],Y[ans[id1+1]]-Y[ans[id1-1]]);
                if((id1+1)%10==0&&!is_prime[ans[id1-1]]) dist*=1.1;
                nx+=dist;
                dist=hypot(X[ans[id1]]-X[ans[id2]],Y[ans[id1]]-Y[ans[id2]]);
                if((id2+1)%10==0&&!is_prime[ans[id1]]) dist*=1.1;
                nx+=dist;
                dist=hypot(X[ans[id1]]-X[ans[id2-1]],Y[ans[id1]]-Y[ans[id2-1]]);
                if((id2)%10==0&&!is_prime[ans[id2-1]]) dist*=1.1;
                nx+=dist;

                for(int i=9-id2%10;i<id1-id2;i+=10){
                    if(i!=id1-id2-1){
                        dist=hypot(X[ans[id2+i+1]]-X[ans[id2+i]],Y[ans[id2+i+1]]-Y[ans[id2+i]]);
                        if(!is_prime[ans[id2+i]]) dist*=0.1,pre+=dist;
                    }
                    if(i!=0){
                        dist=hypot(X[ans[id2+i]]-X[ans[id2+i-1]],Y[ans[id2+i]]-Y[ans[id2+i-1]]);
                        if(!is_prime[ans[id2+i-1]]) dist*=0.1,nx+=dist;
                    }
            
                }

            }


            nxscore+=nx-pre;

            if(curscore>=nxscore){
            //if(forceupdate(curscore-nxscore,curtemp)){
                curscore=nxscore;
                if(id1<id2){
                    FOR(i,id1,id2){
                        swap(ans[i],ans[i+1]);
                        --perm[ans[i]];
                    }
                    perm[ans[id1]]=id2;
                }
                else{
                    for(int i=id1;i>id2;--i){
                        swap(ans[i],ans[i-1]);
                        ++perm[ans[i]];
                    }
                    perm[ans[id1]]=id2;
                }
                if(bestscore>nxscore){
                    bestscore=nxscore;
                    memcpy(bestans,ans,sizeof(ans));

                    cerr<<setprecision(12)<<fixed<<bestscore<<endl;


                }
            }
        }
    }
}
int tans[N+1];
void kopt(int tg){
    double start=timer.get();
    double curscore=bestscore;
    double pre,nx,dist;
    double starttemp,endtemp=0.05,curtemp;
    double limit=timeLimit2;
    //if(tg!=numgrid)starttemp=min(0.05/sqdence[tg],0.5),limit=timeLimit2;
    if(tg!=numgrid)starttemp=min(0.06/sqdence[tg],0.6),limit=timeLimit2;
    //if(tg!=numgrid)starttemp=1.0,limit=timeLimit2;
    //if(tg!=numgrid)starttemp=1.5,limit=20;
    else{
        //return;
        starttemp=0.0001,endtemp=0.00001,limit=60;
    }

    ll turn=0;
    ll cnt=0;
    double t,invtl=1.0/limit;

    while(1){
        ++turn;
        
        
        t=timer.get();
        if(t-start>limit){
            cerr<<turn<<" "<<cnt<<" "<<curscore<<endl;
            return;
        }
        curtemp=1.0/(starttemp+(endtemp-starttemp)*(t-start)*invtl);

        //if(cnt<5) curtemp*=0.3;

        int tid,id1,id2,id3,rsz=rgv[tg].size();
        int sel=rnd.nextInt()%7;
        /*
        if(tg!=numgrid)while(1){
            id1=perm[rgv[tg][rnd.nextInt(rsz)]];
            id2=perm[rgv[tg][rnd.nextInt(rsz)]];
            id3=perm[rgv[tg][rnd.nextInt(rsz)]];
            if(id1==0) id1=N;
            if(id2==0) id2=N;
            if(id3==0) id3=N;
            if(id1!=id2&&id1!=id3&&id2!=id3) break;
        }
        */
       /*
        if(sel==7){
           if(tg!=numgrid)while(1){
                tid=rgv[tg][rnd.nextInt(rsz)];
                id1=perm[tid];
                id2=perm[neigh[tid][rnd.nextInt(neiSZ)]];

                if(id1!=0&&id2!=0&&abs(id1-id2)>1) break;
            }
            else{
                while(1){
                    tid=rnd.nextInt(N-1)+1;
                    id1=perm[tid];
                    id2=perm[neigh[tid][rnd.nextInt(neiSZ)]];

                    if(id1!=0&&id2!=0&abs(id1-id2)>1) break;
                }
            } 
        }
        */
        //else{
            if(tg!=numgrid)while(1){
                tid=rgv[tg][rnd.nextInt(rsz)];
                id1=perm[tid];
                id2=perm[neigh[tid][rnd.nextInt(neiSZ)]];
                id3=perm[neigh[tid][rnd.nextInt(neiSZ)]];
                if(id1==0) id1=N;
                if(id2==0) id2=N;
                if(id3==0) id3=N;

                if(id1!=id2&&id1!=id3&&id2!=id3) break;
            }
            else{
                while(1){
                    tid=rnd.nextInt(N);
                    id1=perm[tid];
                    id2=perm[neigh[tid][rnd.nextInt(neiSZ)]];
                    id3=perm[neigh[tid][rnd.nextInt(neiSZ)]];
                    if(id1==0) id1=N;
                    if(id2==0) id2=N;
                    if(id3==0) id3=N;

                    if(id1!=id2&&id1!=id3&&id2!=id3) break;
                }
            }
            if(id1>id3) swap(id1,id3);
            if(id2>id3) swap(id2,id3);
            if(id1>id2) swap(id1,id2);
        //}

        double nxscore=curscore;
        pre=0,nx=0;

        
        //sel 0-2 2opt sel 3-6 3opt
        if(sel==0){

            pre+=hypot(X[ans[id1]]-X[ans[id1-1]],Y[ans[id1]]-Y[ans[id1-1]]);
            nx+=hypot(X[ans[id2-1]]-X[ans[id1-1]],Y[ans[id2-1]]-Y[ans[id1-1]]);
            if(id1%10==0&&!is_prime[ans[id1-1]]) pre*=1.1,nx*=1.1;

            dist=hypot(X[ans[id2]]-X[ans[id2-1]],Y[ans[id2]]-Y[ans[id2-1]]);
            if(id2%10==0&&!is_prime[ans[id2-1]]) dist*=1.1;
            pre+=dist;
            dist=hypot(X[ans[id2]]-X[ans[id1]],Y[ans[id2]]-Y[ans[id1]]);
            if(id2%10==0&&!is_prime[ans[id1]]) dist*=1.1;
            nx+=dist;

            for(int i=9-id1%10;i<id2-1-id1;i+=10){
                if(!is_prime[ans[id1+i]]){
                    dist=hypot(X[ans[id1+1+i]]-X[ans[id1+i]],Y[ans[id1+1+i]]-Y[ans[id1+i]]);
                    dist*=0.1,pre+=dist;
                }

                if(!is_prime[ans[id2-1-i]]){
                    dist=hypot(X[ans[id2-1-i]]-X[ans[id2-i-2]],Y[ans[id2-1-i]]-Y[ans[id2-i-2]]);
                    dist*=0.1,nx+=dist;
                }
            
            }
            nxscore+=nx-pre;

            //if(curscore>=nxscore){
            if(forceupdate(curscore-nxscore,curtemp)){
                curscore=nxscore;
                rep(i,(id2-id1)/2){
                    swap(ans[id2-1-i],ans[id1+i]);
                    swap(perm[ans[id2-1-i]],perm[ans[id1+i]]);
                }
                if(bestscore>nxscore){
                    bestscore=nxscore;
                    memcpy(bestans,ans,sizeof(ans));

                    cerr<<setprecision(12)<<fixed<<bestscore<<endl;
                }
                ++cnt;
                

            }
        }
        else if(sel==1){
            pre+=hypot(X[ans[id1]]-X[ans[id1-1]],Y[ans[id1]]-Y[ans[id1-1]]);
            nx+=hypot(X[ans[id3-1]]-X[ans[id1-1]],Y[ans[id3-1]]-Y[ans[id1-1]]);
            if(id1%10==0&&!is_prime[ans[id1-1]]) pre*=1.1,nx*=1.1;

            dist=hypot(X[ans[id3]]-X[ans[id3-1]],Y[ans[id3]]-Y[ans[id3-1]]);
            if(id3%10==0&&!is_prime[ans[id3-1]]) dist*=1.1;
            pre+=dist;
            dist=hypot(X[ans[id3]]-X[ans[id1]],Y[ans[id3]]-Y[ans[id1]]);
            if(id3%10==0&&!is_prime[ans[id1]]) dist*=1.1;
            nx+=dist;

            for(int i=9-id1%10;i<id3-id1-1;i+=10){
                if(!is_prime[ans[id1+i]]){
                    dist=hypot(X[ans[id1+1+i]]-X[ans[id1+i]],Y[ans[id1+1+i]]-Y[ans[id1+i]]);
                    dist*=0.1,pre+=dist;
                }

                if(!is_prime[ans[id3-i-1]]){
                    dist=hypot(X[ans[id3-i-1]]-X[ans[id3-i-2]],Y[ans[id3-i-1]]-Y[ans[id3-i-2]]);
                    dist*=0.1,nx+=dist;
                }
            
            }
            nxscore+=nx-pre;

            //if(curscore>=nxscore){
            if(forceupdate(curscore-nxscore,curtemp)){
                curscore=nxscore;
                rep(i,(id3-id1)/2){
                    swap(ans[id3-1-i],ans[id1+i]);
                    swap(perm[ans[id3-1-i]],perm[ans[id1+i]]);
                }
                if(bestscore>nxscore){
                    bestscore=nxscore;
                    memcpy(bestans,ans,sizeof(ans));

                    cerr<<setprecision(12)<<fixed<<bestscore<<endl;
                }
                ++cnt;

            }
        }
        else if(sel==2){
            pre+=hypot(X[ans[id2]]-X[ans[id2-1]],Y[ans[id2]]-Y[ans[id2-1]]);
            nx+=hypot(X[ans[id3-1]]-X[ans[id2-1]],Y[ans[id3-1]]-Y[ans[id2-1]]);
            if(id2%10==0&&!is_prime[ans[id2-1]]) pre*=1.1,nx*=1.1;

            dist=hypot(X[ans[id3]]-X[ans[id3-1]],Y[ans[id3]]-Y[ans[id3-1]]);
            if(id3%10==0&&!is_prime[ans[id3-1]]) dist*=1.1;
            pre+=dist;
            dist=hypot(X[ans[id3]]-X[ans[id2]],Y[ans[id3]]-Y[ans[id2]]);
            if(id3%10==0&&!is_prime[ans[id2]]) dist*=1.1;
            nx+=dist;

            for(int i=9-id2%10;i<id3-id2-1;i+=10){
                if(!is_prime[ans[id2+i]]){
                    dist=hypot(X[ans[id2+1+i]]-X[ans[id2+i]],Y[ans[id2+1+i]]-Y[ans[id2+i]]);
                    dist*=0.1,pre+=dist;
                }

                if(!is_prime[ans[id3-i-1]]){
                    dist=hypot(X[ans[id3-i-1]]-X[ans[id3-i-2]],Y[ans[id3-i-1]]-Y[ans[id3-i-2]]);
                    dist*=0.1,nx+=dist;
                }
            
            }
            nxscore+=nx-pre;

            //if(curscore>=nxscore){
            if(forceupdate(curscore-nxscore,curtemp)){
                curscore=nxscore;
                rep(i,(id3-id2)/2){
                    swap(ans[id3-1-i],ans[id2+i]);
                    swap(perm[ans[id3-1-i]],perm[ans[id2+i]]);
                }
                
                if(bestscore>nxscore){
                    bestscore=nxscore;
                    memcpy(bestans,ans,sizeof(ans));

                    cerr<<setprecision(12)<<fixed<<bestscore<<endl;
                }
                ++cnt;

            }
        }
        else if(sel==3){

            dist=hypot(X[ans[id1]]-X[ans[id1-1]],Y[ans[id1]]-Y[ans[id1-1]]);
            if(id1%10==0&&!is_prime[ans[id1-1]]) dist*=1.1;
            pre+=dist;
            dist=hypot(X[ans[id2]]-X[ans[id2-1]],Y[ans[id2]]-Y[ans[id2-1]]);
            if(id2%10==0&&!is_prime[ans[id2-1]]) dist*=1.1;
            pre+=dist;
            dist=hypot(X[ans[id3]]-X[ans[id3-1]],Y[ans[id3]]-Y[ans[id3-1]]);
            if(id3%10==0&&!is_prime[ans[id3-1]]) dist*=1.1;
            pre+=dist;

            dist=hypot(X[ans[id2-1]]-X[ans[id1-1]],Y[ans[id2-1]]-Y[ans[id1-1]]);
            if(id1%10==0&&!is_prime[ans[id1-1]]) dist*=1.1;
            nx+=dist;
            dist=hypot(X[ans[id3-1]]-X[ans[id1]],Y[ans[id3-1]]-Y[ans[id1]]);
            if(id2%10==0&&!is_prime[ans[id1]]) dist*=1.1;
            nx+=dist;
            dist=hypot(X[ans[id3]]-X[ans[id2]],Y[ans[id3]]-Y[ans[id2]]);
            if(id3%10==0&&!is_prime[ans[id2]]) dist*=1.1;
            nx+=dist;

            for(int i=9-id1%10;i<id2-1-id1;i+=10){
                if(!is_prime[ans[id1+i]]){
                    dist=hypot(X[ans[id1+1+i]]-X[ans[id1+i]],Y[ans[id1+1+i]]-Y[ans[id1+i]]);
                    dist*=0.1,pre+=dist;
                }
                if(!is_prime[ans[id2-i-1]]){
                    dist=hypot(X[ans[id2-1-i]]-X[ans[id2-i-2]],Y[ans[id2-1-i]]-Y[ans[id2-i-2]]);
                    dist*=0.1,nx+=dist;
                }
            }

            for(int i=9-id2%10;i<id3-1-id2;i+=10){
                if(!is_prime[ans[id2+i]]){
                    dist=hypot(X[ans[id2+1+i]]-X[ans[id2+i]],Y[ans[id2+1+i]]-Y[ans[id2+i]]);
                    dist*=0.1,pre+=dist;
                }
                if(!is_prime[ans[id3-i-1]]){
                    dist=hypot(X[ans[id3-1-i]]-X[ans[id3-i-2]],Y[ans[id3-1-i]]-Y[ans[id3-i-2]]);
                    dist*=0.1,nx+=dist;
                }
            }

            nxscore+=nx-pre;

            //if(curscore>=nxscore){
            if(forceupdate(curscore-nxscore,curtemp)){
                curscore=nxscore;
                rep(i,(id2-id1)/2){
                    swap(ans[id2-1-i],ans[id1+i]);
                    swap(perm[ans[id2-1-i]],perm[ans[id1+i]]);
                }
                rep(i,(id3-id2)/2){
                    swap(ans[id3-1-i],ans[id2+i]);
                    swap(perm[ans[id3-1-i]],perm[ans[id2+i]]);
                }
                if(bestscore>nxscore){
                    bestscore=nxscore;
                    memcpy(bestans,ans,sizeof(ans));

                    cerr<<setprecision(12)<<fixed<<bestscore<<endl;
                }
                ++cnt;

            }
        }
        else if(sel==4){

            dist=hypot(X[ans[id1]]-X[ans[id1-1]],Y[ans[id1]]-Y[ans[id1-1]]);
            if(id1%10==0&&!is_prime[ans[id1-1]]) dist*=1.1;
            pre+=dist;
            
            dist=hypot(X[ans[id2]]-X[ans[id2-1]],Y[ans[id2]]-Y[ans[id2-1]]);
            if(id2%10==0&&!is_prime[ans[id2-1]]) dist*=1.1;
            pre+=dist;
            
            dist=hypot(X[ans[id3]]-X[ans[id3-1]],Y[ans[id3]]-Y[ans[id3-1]]);
            if(id3%10==0&&!is_prime[ans[id3-1]]) dist*=1.1;
            pre+=dist;

            dist=hypot(X[ans[id2]]-X[ans[id1-1]],Y[ans[id2]]-Y[ans[id1-1]]);
            if(id1%10==0&&!is_prime[ans[id1-1]]) dist*=1.1;
            nx+=dist;
            dist=hypot(X[ans[id1]]-X[ans[id3-1]],Y[ans[id1]]-Y[ans[id3-1]]);
            if((id1+id3-id2)%10==0&&!is_prime[ans[id3-1]]) dist*=1.1;
            nx+=dist;
            dist=hypot(X[ans[id3]]-X[ans[id2-1]],Y[ans[id3]]-Y[ans[id2-1]]);
            if(id3%10==0&&!is_prime[ans[id2-1]]) dist*=1.1;
            nx+=dist;

            for(int i=9-id1%10;i<id3-id1-1;i+=10){
                if(!is_prime[ans[id1+i]]&&i!=id2-1-id1){
                    dist=hypot(X[ans[id1+1+i]]-X[ans[id1+i]],Y[ans[id1+1+i]]-Y[ans[id1+i]]);
                    dist*=0.1,pre+=dist;
                }
                if(i<id3-1-id2){
                    if(!is_prime[ans[id2+i]]){
                        dist=hypot(X[ans[id2+1+i]]-X[ans[id2+i]],Y[ans[id2+1+i]]-Y[ans[id2+i]]);
                        dist*=0.1,nx+=dist;
                    }
                }
                else if(i>id3-1-id2){
                    int ti=i-id3+id2;
                    if(!is_prime[ans[id1+ti]]){
                        dist=hypot(X[ans[id1+1+ti]]-X[ans[id1+ti]],Y[ans[id1+1+ti]]-Y[ans[id1+ti]]);
                        dist*=0.1,nx+=dist;
                    }
                }
                
            }

            nxscore+=nx-pre;

            //if(curscore>=nxscore){
            if(forceupdate(curscore-nxscore,curtemp)){
                curscore=nxscore;
                memcpy(tans,ans,sizeof(ans));
                rep(i,id3-id1){
                    if(i<=id3-1-id2){
                        ans[id1+i]=tans[id2+i];
                        perm[tans[id2+i]]=id1+i;
                    }
                    else if(i>id3-1-id2){
                        int ti=i-id3+id2;
                        ans[id1+i]=tans[id1+ti];
                        perm[tans[id1+ti]]=id1+i;
                    }
                }
                if(bestscore>nxscore){
                    bestscore=nxscore;
                    memcpy(bestans,ans,sizeof(ans));

                    cerr<<setprecision(12)<<fixed<<bestscore<<endl;
                }
                ++cnt;

            }

        }
        else if(sel==5){

            dist=hypot(X[ans[id1]]-X[ans[id1-1]],Y[ans[id1]]-Y[ans[id1-1]]);
            if(id1%10==0&&!is_prime[ans[id1-1]]) dist*=1.1;
            pre+=dist;
            
            dist=hypot(X[ans[id2]]-X[ans[id2-1]],Y[ans[id2]]-Y[ans[id2-1]]);
            if(id2%10==0&&!is_prime[ans[id2-1]]) dist*=1.1;
            pre+=dist;
            
            dist=hypot(X[ans[id3]]-X[ans[id3-1]],Y[ans[id3]]-Y[ans[id3-1]]);
            if(id3%10==0&&!is_prime[ans[id3-1]]) dist*=1.1;
            pre+=dist;

            dist=hypot(X[ans[id2]]-X[ans[id1-1]],Y[ans[id2]]-Y[ans[id1-1]]);
            if(id1%10==0&&!is_prime[ans[id1-1]]) dist*=1.1;
            nx+=dist;
            dist=hypot(X[ans[id2-1]]-X[ans[id3-1]],Y[ans[id2-1]]-Y[ans[id3-1]]);
            if((id1+id3-id2)%10==0&&!is_prime[ans[id3-1]]) dist*=1.1;
            nx+=dist;
            dist=hypot(X[ans[id3]]-X[ans[id1]],Y[ans[id3]]-Y[ans[id1]]);
            if(id3%10==0&&!is_prime[ans[id1]]) dist*=1.1;
            nx+=dist;

            for(int i=9-id1%10;i<id3-id1-1;i+=10){
                if(!is_prime[ans[id1+i]]&&i!=id2-1-id1){
                    dist=hypot(X[ans[id1+1+i]]-X[ans[id1+i]],Y[ans[id1+1+i]]-Y[ans[id1+i]]);
                    dist*=0.1,pre+=dist;
                }
                if(i<id3-1-id2){
                    if(!is_prime[ans[id2+i]]){
                        dist=hypot(X[ans[id2+1+i]]-X[ans[id2+i]],Y[ans[id2+1+i]]-Y[ans[id2+i]]);
                        dist*=0.1,nx+=dist;
                    }
                }
                else if(i>id3-1-id2){
                    int ti=i-id3+id2;
                    if(!is_prime[ans[id2-1-ti]]){
                        dist=hypot(X[ans[id2-1-ti]]-X[ans[id2-ti-2]],Y[ans[id2-1-ti]]-Y[ans[id2-ti-2]]);
                        dist*=0.1,nx+=dist;
                    }
                }
                
            }

            nxscore+=nx-pre;

            //if(curscore>=nxscore){
            if(forceupdate(curscore-nxscore,curtemp)){
                curscore=nxscore;
                memcpy(tans,ans,sizeof(ans));
                rep(i,id3-id1){
                    if(i<=id3-1-id2){
                        ans[id1+i]=tans[id2+i];
                        perm[tans[id2+i]]=id1+i;
                    }
                    else if(i>id3-1-id2){
                        int ti=i-id3+id2;
                        ans[id1+i]=tans[id2-ti-1];
                        perm[tans[id2-ti-1]]=id1+i;
                    }
                }
                if(bestscore>nxscore){
                    bestscore=nxscore;
                    memcpy(bestans,ans,sizeof(ans));

                    cerr<<setprecision(12)<<fixed<<bestscore<<endl;
                }
                ++cnt;

            }

        }
        else if(sel==6){

            dist=hypot(X[ans[id1]]-X[ans[id1-1]],Y[ans[id1]]-Y[ans[id1-1]]);
            if(id1%10==0&&!is_prime[ans[id1-1]]) dist*=1.1;
            pre+=dist;
            
            dist=hypot(X[ans[id2]]-X[ans[id2-1]],Y[ans[id2]]-Y[ans[id2-1]]);
            if(id2%10==0&&!is_prime[ans[id2-1]]) dist*=1.1;
            pre+=dist;
            
            dist=hypot(X[ans[id3]]-X[ans[id3-1]],Y[ans[id3]]-Y[ans[id3-1]]);
            if(id3%10==0&&!is_prime[ans[id3-1]]) dist*=1.1;
            pre+=dist;

            dist=hypot(X[ans[id3-1]]-X[ans[id1-1]],Y[ans[id3-1]]-Y[ans[id1-1]]);
            if(id1%10==0&&!is_prime[ans[id1-1]]) dist*=1.1;
            nx+=dist;
            dist=hypot(X[ans[id1]]-X[ans[id2]],Y[ans[id1]]-Y[ans[id2]]);
            if((id1+id3-id2)%10==0&&!is_prime[ans[id2]]) dist*=1.1;
            nx+=dist;
            dist=hypot(X[ans[id3]]-X[ans[id2-1]],Y[ans[id3]]-Y[ans[id2-1]]);
            if(id3%10==0&&!is_prime[ans[id2-1]]) dist*=1.1;
            nx+=dist;

            for(int i=9-id1%10;i<id3-id1-1;i+=10){
                if(!is_prime[ans[id1+i]]&&i!=id2-1-id1){
                    dist=hypot(X[ans[id1+1+i]]-X[ans[id1+i]],Y[ans[id1+1+i]]-Y[ans[id1+i]]);
                    dist*=0.1,pre+=dist;
                }
                if(i<id3-1-id2){
                    if(!is_prime[ans[id3-i-1]]){
                        dist=hypot(X[ans[id3-1-i]]-X[ans[id3-i-2]],Y[ans[id3-1-i]]-Y[ans[id3-i-2]]);
                        dist*=0.1,nx+=dist;
                    }
                }
                else if(i>id3-1-id2){
                    int ti=i-id3+id2;
                    if(!is_prime[ans[id1+ti]]){
                        dist=hypot(X[ans[id1+1+ti]]-X[ans[id1+ti]],Y[ans[id1+1+ti]]-Y[ans[id1+ti]]);
                        dist*=0.1,nx+=dist;
                    }
                }
                
            }

            nxscore+=nx-pre;

            //if(curscore>=nxscore){
            if(forceupdate(curscore-nxscore,curtemp)){
                curscore=nxscore;
                memcpy(tans,ans,sizeof(ans));
                rep(i,id3-id1){
                    if(i<=id3-1-id2){
                        ans[id1+i]=tans[id3-i-1];
                        perm[tans[id3-i-1]]=id1+i;
                    }
                    else if(i>id3-1-id2){
                        int ti=i-id3+id2;
                        ans[id1+i]=tans[id1+ti];
                        perm[tans[id1+ti]]=id1+i;
                    }
                }
                if(bestscore>nxscore){
                    bestscore=nxscore;
                    memcpy(bestans,ans,sizeof(ans));

                    cerr<<setprecision(12)<<fixed<<bestscore<<endl;
                }
                ++cnt;

            } 
            
        }
        /*
        else if(sel==7){
            dist=hypot(X[ans[id1]]-X[ans[id1-1]],Y[ans[id1]]-Y[ans[id1-1]]);
            if(id1%10==0&&!is_prime[ans[id1-1]]) dist*=1.1;
            pre+=dist;
            dist=hypot(X[ans[id1+1]]-X[ans[id1]],Y[ans[id1+1]]-Y[ans[id1]]);
            if((id1+1)%10==0&&!is_prime[ans[id1]]) dist*=1.1;
            pre+=dist;
            dist=hypot(X[ans[id2]]-X[ans[id2-1]],Y[ans[id2]]-Y[ans[id2-1]]);
            if(id2%10==0&&!is_prime[ans[id2-1]]) dist*=1.1;
            pre+=dist;
            dist=hypot(X[ans[id2+1]]-X[ans[id2]],Y[ans[id2+1]]-Y[ans[id2]]);
            if((id2+1)%10==0&&!is_prime[ans[id2]]) dist*=1.1;
            pre+=dist;

            dist=hypot(X[ans[id2]]-X[ans[id1-1]],Y[ans[id2]]-Y[ans[id1-1]]);
            if(id1%10==0&&!is_prime[ans[id1-1]]) dist*=1.1;
            nx+=dist;
            dist=hypot(X[ans[id1+1]]-X[ans[id2]],Y[ans[id1+1]]-Y[ans[id2]]);
            if((id1+1)%10==0&&!is_prime[ans[id2]]) dist*=1.1;
            nx+=dist;
            dist=hypot(X[ans[id1]]-X[ans[id2-1]],Y[ans[id1]]-Y[ans[id2-1]]);
            if(id2%10==0&&!is_prime[ans[id2-1]]) dist*=1.1;
            nx+=dist;
            dist=hypot(X[ans[id2+1]]-X[ans[id1]],Y[ans[id2+1]]-Y[ans[id1]]);
            if((id2+1)%10==0&&!is_prime[ans[id1]]) dist*=1.1;
            nx+=dist;


            nxscore+=nx-pre;

            //if(curscore>=nxscore){
            if(forceupdate(curscore-nxscore,curtemp)){
                curscore=nxscore;
                swap(ans[id1],ans[id2]);
                swap(perm[ans[id1]],perm[ans[id2]]);
                if(bestscore>nxscore){
                    bestscore=nxscore;
                    memcpy(bestans,ans,sizeof(ans));

                    cerr<<setprecision(12)<<fixed<<bestscore<<endl;
                }
                ++cnt;
            } 
        }
        */
    }
}

int main(){
    ios::sync_with_stdio(false),cin.tie(0),cout.tie(0);
    
    rep(i,N){
        cin>>X[i]>>Y[i];
        rgv[(int)Y[i]/uY*RX+(int)X[i]/uX].pb(i);
    }
    rep(i,numgrid) sqdence[i]=sqrt((double)rgv[i].size()/(uX*uY));

    double score=0;
    //score=init();
    score=init2();
    //score=init1_2();
    bestscore=score;
    memcpy(bestans,ans,sizeof(ans));
    rep(i,N) perm[ans[i]]=i;

    timer.reset();
    while(timer.get()<=timeLimit){
        memcpy(ans,bestans,sizeof(bestans));
        rep(i,N) perm[bestans[i]]=i;
        int nxgrid=rnd.nextInt(numgrid+1);
        //int nxgrid=numgrid;
        cerr<<nxgrid<<" "<<sqdence[nxgrid]<<endl;
        if(nxgrid!=numgrid&&rgv[nxgrid].size()<=5) continue;
        //improve1(nxgrid);
        //improve2(nxgrid);
        //improve3(nxgrid);
        kopt(nxgrid);
    }
    

    cout<<setprecision(12)<<fixed<<bestscore<<endl;
    cerr<<setprecision(12)<<fixed<<bestscore<<endl;
    rep(i,N+1) cout<<bestans[i]<<endl;
    return 0;
}
