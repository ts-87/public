#include <bits/stdc++.h>

using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

double timeLimit=2.98;
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
int N,M,L,M2;
const int dx[]={0,-1,0,1},dy[]={-1,0,1,0};
int point[500]={10,-7,-2,-1};
vector<int> S[500];
int8_t cnt[29][29];
vector<string> bestans,ans;
const string item=".LR";
const int ISZ=3;
int bestscore=-1,curscore=0;
int ttx,tty;

bool visit[500][29][29],previsit[500][29][29];
int src[500],presrc[500];
int getscore(){
    int ret=0;
    memset(cnt,0,sizeof(cnt));
    rep(i,N){
        int tx,ty;
        if(!visit[i][tty][ttx]){
            ty=src[i]/M,tx=src[i]%M;
            ret+=point[cnt[ty][tx]++];
            continue;
        }
        memset(visit[i],0,sizeof(visit[i]));
        int cx=14,cy=14;
        int cur=0;
        
        visit[i][cy][cx]=true;
        rep(j,S[i].size()){
            
            if(ans[cy][cx]=='.'){
                if(S[i][j]<1000) cur=(cur+S[i][j]+400)&3,visit[i][cy][cx]=true;
                else{
                    
                    tx=cx+dx[cur]*S[i][j]/1000,ty=cy+dy[cur]*S[i][j]/1000;
                    cx=max(1,min(tx,27));
                    cy=max(1,min(ty,27));
                    //if(ans[ty][tx]!='#') cx=tx,cy=ty;
                    
                }
            }
            else if(ans[cy][cx]=='L'){
                if(S[i][j]<1000) cur=(cur+abs(S[i][j]))&3,visit[i][cy][cx]=true;
                
                else{
                    tx=cx+dx[cur]*S[i][j]/1000,ty=cy+dy[cur]*S[i][j]/1000;
                    cx=max(1,min(tx,27));
                    cy=max(1,min(ty,27));
                    //if(ans[ty][tx]!='#') cx=tx,cy=ty;
                }
            }
            else{
                if(S[i][j]<1000) cur=(cur+abs(S[i][j])*3)&3,visit[i][cy][cx]=true;
                
                else{
                    tx=cx+dx[cur]*S[i][j]/1000,ty=cy+dy[cur]*S[i][j]/1000;
                    cx=max(1,min(tx,27));
                    cy=max(1,min(ty,27));
                    //if(ans[ty][tx]!='#') cx=tx,cy=ty;
                }
            }
            
            //visit[i][cy][cx]=true;
        }
        ret+=point[cnt[cy][cx]++];
        src[i]=cy*M+cx;
    }

    return ret;
}
inline bool forceupdate(double sub,double temp){
    if(sub>=0) return true;
    double d=(sub)*temp;
    if(d<-6) return false;
    return exp(d)>rnd.nextDouble();
}
void improve(){
    int curscore=bestscore;
    double sastart=timer.get();
    double starttemp=2.0,endtemp=0.1,curtemp;
    int turn=0;
    double t;
    double invtl=1.0/timeLimit;
    while(1){
        ++turn;
        t=timer.get();
        if(t>timeLimit){
            cerr<<turn<<endl;
            return;
        }
        curtemp=1.0/(starttemp+(endtemp-starttemp)*(t-sastart)*invtl);
        int prescore=curscore;

        ttx=rnd.nextInt(M2-1)+1,tty=rnd.nextInt(M2-1)+1;
        char preit=ans[tty][ttx];
        int it;
        
        it=rnd.nextInt(ISZ);

        if(item[it]==preit) it=(it+2)%3;
        
        ans[tty][ttx]=item[it];
        curscore=getscore();
        if(forceupdate(curscore-prescore,curtemp)){
            if(bestscore<curscore){
                bestscore=curscore;
                bestans=ans;
                //memcpy(previsit,visit,sizeof(visit));
                //memcpy(presrc,src,sizeof(src));
                //cerr<<timer.get()<<endl;
            }
            memcpy(previsit,visit,sizeof(visit));
            memcpy(presrc,src,sizeof(src));
        }
        else{
            memcpy(visit,previsit,sizeof(previsit));
            memcpy(src,presrc,sizeof(presrc));
            curscore=prescore,ans[tty][ttx]=preit;
        }

    }
    return;
}
int main(){
    cin>>N>>M>>L;
    M2=M-1;
    timer.reset();
    ans.resize(M);
    bestans.resize(M);
    string s;
    rep(i,N){
        cin>>s;
        int ct=0;
        rep(j,L){
            if(s[j]=='S'){
                if(j>0&&ct<1000)S[i].pb(ct),ct=1000;
                else ct+=1000;
            }
            else if(s[j]=='L'){
                if(j>0&&(ct<=0||ct>=1000)) S[i].pb(ct),ct=1;
                else ++ct;
            }
            else{
                if(j>0&&ct>=0) S[i].pb(ct),ct=-1;
                else --ct;
            }
        }
        S[i].pb(ct);
    }
    rep(i,M){
        if(i==0||i==M-1)ans[i]=string(M,'#');
        else ans[i]=string(M,'L');
    }
    rep(i,M) ans[i][0]='#',ans[i][M-1]='#';

    memset(visit,-1,sizeof(visit));
    /*
    FOR(i,1,M2)FOR(j,1,M2){
        //ans[i][j]=item[rnd.nextInt(ISZ)];
        ans[i][j]='L';  
    }
    */
    curscore=getscore();
    
    if(bestscore<curscore){
        bestscore=curscore;
        bestans=ans;
        memcpy(previsit,visit,sizeof(visit));
        memcpy(presrc,src,sizeof(src));
    }
    cerr<<bestscore<<endl; 
    improve();
    cerr<<bestscore<<endl;
    //rep(i,M) cout<<bestans[i]<<endl;
    rep(i,M) cout<<bestans[i]<<"\r\n";
    return 0;
}
