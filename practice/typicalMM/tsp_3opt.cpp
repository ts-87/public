#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

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


Timer timer;
int N;
int px[1001],py[1001];
vector<int> ans,bestans,tans;
bool used[1001];
double bestscore;
//N<=1000 small N
double dist[1001][1001];

inline bool forceupdate(double diff,double temp){
    if(diff>=0) return true;
    double d=(diff)*temp;
    if(d<-6) return false;
    return exp(d)>rnd.nextDouble();
    
}
void SA3opt(){
    double t,curscore=bestscore;
    //double pre1,pre2,nx1,nx2;
    double starttemp=2,endtemp=0.2,curtemp;
    int turn=0;
    double invtl=1.0/timeLimit;
    while(1){
        ++turn;
        t=timer.get();
        if(t>timeLimit){
            cerr<<turn<<" "<<bestscore<<endl;
            return;
        }
        curtemp=1.0/(starttemp+(endtemp-starttemp)*t*invtl);
        int id1,id2,id3;
        int sel=rnd.nextInt()%7;
        while(1){
            id1=rnd.nextInt(N)+1,id2=rnd.nextInt(N)+1,id3=rnd.nextInt(N)+1;
            /*
            if(id1==0) id1=N;
            if(id2==0) id2=N;
            if(id3==0) id3=N;
            */
            if(id1!=id2&&id1!=id3&&id2!=id3) break;
        }

        if(id1>id3) swap(id1,id3);
        if(id2>id3) swap(id2,id3);
        if(id1>id2) swap(id1,id2);

        double pre=0,nx=0;
        if(sel==0){
            pre+=dist[ans[id1]][ans[id1-1]];
            nx+=dist[ans[id2-1]][ans[id1-1]];
            pre+=dist[ans[id2]][ans[id2-1]];
            nx+=dist[ans[id2]][ans[id1]];
            /*
            pre+=hypot(px[ans[id1]]-px[ans[id1-1]],py[ans[id1]]-py[ans[id1-1]]);
            nx+=hypot(px[ans[id2-1]]-px[ans[id1-1]],py[ans[id2-1]]-py[ans[id1-1]]);
            pre+=hypot(px[ans[id2]]-px[ans[id2-1]],py[ans[id2]]-py[ans[id2-1]]);
            nx+=hypot(px[ans[id2]]-px[ans[id1]],py[ans[id2]]-py[ans[id1]]);
            */
            if(forceupdate(pre-nx,curtemp)){
                curscore+=nx-pre;
                reverse(ans.begin()+id1,ans.begin()+id2);
                if(bestscore>curscore){
                    bestscore=curscore;
                    bestans=ans;
                }
            }
        }
        else if(sel==1){
            pre+=dist[ans[id1]][ans[id1-1]];
            nx+=dist[ans[id3-1]][ans[id1-1]];
            pre+=dist[ans[id3]][ans[id3-1]];
            nx+=dist[ans[id3]][ans[id1]];
            if(forceupdate(pre-nx,curtemp)){
                curscore+=nx-pre;
                reverse(ans.begin()+id1,ans.begin()+id3);
                if(bestscore>curscore){
                    bestscore=curscore;
                    bestans=ans;
                }
            }
        }
        else if(sel==2){
            pre+=dist[ans[id2]][ans[id2-1]];
            nx+=dist[ans[id3-1]][ans[id2-1]];
            pre+=dist[ans[id3]][ans[id3-1]];
            nx+=dist[ans[id3]][ans[id2]];
            if(forceupdate(pre-nx,curtemp)){
                curscore+=nx-pre;
                reverse(ans.begin()+id2,ans.begin()+id3);
                if(bestscore>curscore){
                    bestscore=curscore;
                    bestans=ans;
                }
            }
        }
        else if(sel==3){
            pre+=dist[ans[id1]][ans[id1-1]];
            pre+=dist[ans[id2]][ans[id2-1]];
            pre+=dist[ans[id3]][ans[id3-1]];
            nx+=dist[ans[id2-1]][ans[id1-1]];
            nx+=dist[ans[id3-1]][ans[id1]];
            nx+=dist[ans[id3]][ans[id2]];
            if(forceupdate(pre-nx,curtemp)){
                curscore+=nx-pre;
                reverse(ans.begin()+id1,ans.begin()+id2);
                reverse(ans.begin()+id2,ans.begin()+id3);
                if(bestscore>curscore){
                    bestscore=curscore;
                    bestans=ans;
                }
            }
        }
        else if(sel==4){
            pre+=dist[ans[id1]][ans[id1-1]];
            pre+=dist[ans[id2]][ans[id2-1]];
            pre+=dist[ans[id3]][ans[id3-1]];
            nx+=dist[ans[id2]][ans[id1-1]];
            nx+=dist[ans[id3-1]][ans[id1]];
            nx+=dist[ans[id3]][ans[id2-1]];
            if(forceupdate(pre-nx,curtemp)){
                curscore+=nx-pre;
                tans=ans;
                rep(i,id3-id1){
                    if(i<=id3-1-id2){
                        ans[id1+i]=tans[id2+i];
                    }
                    else if(i>id3-1-id2){
                        int ti=i-id3+id2;
                        ans[id1+i]=tans[id1+ti];
                    }
                }
                if(bestscore>curscore){
                    bestscore=curscore;
                    bestans=ans;
                }
            }
        }
        else if(sel==5){
            pre+=dist[ans[id1]][ans[id1-1]];
            pre+=dist[ans[id2]][ans[id2-1]];
            pre+=dist[ans[id3]][ans[id3-1]];
            nx+=dist[ans[id2]][ans[id1-1]];
            nx+=dist[ans[id3]][ans[id1]];
            nx+=dist[ans[id3-1]][ans[id2-1]];
            if(forceupdate(pre-nx,curtemp)){
                curscore+=nx-pre;
                tans=ans;
                rep(i,id3-id1){
                    if(i<=id3-1-id2){
                        ans[id1+i]=tans[id2+i];
                    }
                    else if(i>id3-1-id2){
                        int ti=i-id3+id2;
                        ans[id1+i]=tans[id2-ti-1];
                    }
                }
                if(bestscore>curscore){
                    bestscore=curscore;
                    bestans=ans;
                }
            }
        }
        else if(sel==6){
            pre+=dist[ans[id1]][ans[id1-1]];
            pre+=dist[ans[id2]][ans[id2-1]];
            pre+=dist[ans[id3]][ans[id3-1]];
            nx+=dist[ans[id3-1]][ans[id1-1]];
            nx+=dist[ans[id2]][ans[id1]];
            nx+=dist[ans[id3]][ans[id2-1]];
            if(forceupdate(pre-nx,curtemp)){
                curscore+=nx-pre;
                tans=ans;
                rep(i,id3-id1){
                    if(i<=id3-1-id2){
                        ans[id1+i]=tans[id3-i-1];
                    }
                    else if(i>id3-1-id2){
                        int ti=i-id3+id2;
                        ans[id1+i]=tans[id1+ti];
                    }
                }
                if(bestscore>curscore){
                    bestscore=curscore;
                    bestans=ans;
                }
            }
        }
    }
    return;
}

void SA2opt(){
    double t,curscore=bestscore;
    double pre1,pre2,nx1,nx2;
    double starttemp=2,endtemp=0.2,curtemp;
    int turn=0;
    double invtl=1.0/timeLimit;
    while(1){
        ++turn;
        t=timer.get();
        if(t>timeLimit){
            cerr<<turn<<" "<<bestscore<<endl;
            return;
        }
        curtemp=1.0/(starttemp+(endtemp-starttemp)*t*invtl);
        int id1,id2;
        while(1){
            id1=rnd.nextInt(N),id2=rnd.nextInt(N);
            int sub=abs(id1-id2);
            if(sub>1&&sub<N-1) break;
        }
        int p1=ans[id1],p2=ans[id2],q1=(id1+1)%N,q2=(id2+N-1)%N;
        double nxscore=curscore;
        pre1=hypot(px[p1]-px[ans[q1]],py[p1]-py[ans[q1]]);
        pre2=hypot(px[p2]-px[ans[q2]],py[p2]-py[ans[q2]]);
        nx1=hypot(px[p1]-px[ans[q2]],py[p1]-py[ans[q2]]);
        nx2=hypot(px[p2]-px[ans[q1]],py[p2]-py[ans[q1]]);
        nxscore+=nx1+nx2-pre1-pre2;
        //if(curscore>=nxscore){
        if(forceupdate(curscore-nxscore,curtemp)){
            curscore=nxscore;
            if(id1<id2) reverse(ans.begin()+(id1+1),ans.begin()+(id2));
            else{
                int l=(id1+1)%N,r=(id2+N-1)%N;
                while(1){
                    swap(ans[l],ans[r]);
                    if(abs(r-l)<=1) break;
                    ++l;
                    l%=N;r=(r+N-1)%N;
                }

            }
            if(bestscore>nxscore){
                bestscore=nxscore;
                bestans=ans;
            }
        }
    }
    return;
}

int main(){
    cin>>N;
    rep(i,N) cin>>px[i]>>py[i];
    timer.reset();
    rep(i,N+1)rep(j,N+1){
        dist[i][j]=hypot(px[i%N]-px[j%N],py[i%N]-py[j%N]);
    }
    double curscore=0;
    int cur=0;
    ans.pb(cur);
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
            ans.pb(nx),cur=nx,curscore+=mn,used[nx]=true;
            
        }
        else break;
    }
    curscore+=hypot(px[cur]-px[0],py[cur]-py[0]);
    ans.pb(0);
    bestscore=curscore;
    bestans=ans;

    //SA2opt();
    SA3opt();
    //0-index
    rep(i,N) cout<<bestans[i]<<endl;
    return 0;
}