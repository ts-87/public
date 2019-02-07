#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

double timeLimit=0.85;
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
Timer timer;
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
const int H=10,W=19;
const int SZ=10000;
const string direction="URDL";
const int dx[]={0,1,0,-1},dy[]={-1,0,1,0};
//int8_t grid[10][19],bestgrid[10][19],initgrid[10][19],tmpgrid[10][19],basegrid[10][19];
int8_t grid[SZ][190],bestgrid[190],tmpgrid[190];
ll used[SZ][190],bestused[190],tused[190];
//ll used[10][19],bestused[10][19],baseused[10][19];
int R;
int px[SZ][10],py[SZ][10],dir[SZ][10];
vector<int8_t> alv[SZ];
bool ng[10];
bool glp=true;
int bestscore=0,curscore=0,basescore=0;

struct State{
    int id,eval,cur,hash;
    State(){};
    State(int id,int eval,int cur,ll hash):id(id),eval(eval),cur(cur),hash(hash){};
    bool operator<(const State &r)const{return eval>r.eval;}
    bool operator==(const State &r)const{return hash==r.hash;}
};
int tpx[10],tpy[10],tdir[10];
int bfs(bool dirp,int nid){
    
    int ret=0;
    memset(ng,-1,sizeof(ng));
    int sz=alv[nid].size();
    rep(i,sz){
        ng[alv[nid][i]]=false;
    }
    memcpy(tused,used[nid],sizeof(used[nid]));
    memcpy(tmpgrid,grid[nid],sizeof(grid[nid]));
    
    rep(i,R) tpx[i]=px[nid][i],tpy[i]=py[nid][i],tdir[i]=dir[nid][i];
    while(1){

        int tsc=0;

            rep(j,R)if(!ng[j]){
                int td=tdir[j];
                int tx=(tpx[j]+dx[td]+W)%W,ty=(tpy[j]+dy[td]+H)%H;
                int nxd=tmpgrid[ty*W+tx]>0?tmpgrid[ty*W+tx]%10-1:td;
                if(tmpgrid[ty*W+tx]==-1||((tused[ty*W+tx]>>(j*4))&(1<<nxd))){
                    if(tmpgrid[tpy[j]*W+tpx[j]]!=0||tused[tpy[j]*W+tpx[j]]!=0){
                        ng[j]=true;
                        continue;
                    }
                    rep(kk,4){
                        int k;
                        if(dirp) k=(td+kk+3)&3;
                        else k=(td-kk+5)&3;
                        if(k!=td){
                        int ttx=(tpx[j]+dx[k]+W)%W,tty=(tpy[j]+dy[k]+H)%H;
                        nxd=tmpgrid[tty*W+ttx]>0?tmpgrid[tty*W+ttx]%10-1:k;
                        if(tmpgrid[tty*W+ttx]!=-1&&((tused[tty*W+ttx]>>(j*4))&(1<<nxd))==0){
                            
                            ++tsc;
                            tmpgrid[tpy[j]*W+tpx[j]]=11+k;
                            //used[tty][ttx]|=(1<<dir[j])<<(j*4);
                            tused[tpy[j]*W+tpx[j]]|=1ll<<(k+j*4);
                            tdir[j]=nxd;
                            tpy[j]=tty,tpx[j]=ttx;
                            break;
                        }
                        }
                        if(kk==3)ng[j]=true;
                    }
                }
                else{
                    
                    if(tmpgrid[tpy[j]*W+tpx[j]]>0) tdir[j]=tmpgrid[tpy[j]*W+tpx[j]]%10-1;
                    tused[tpy[j]*W+tpx[j]]|=1ll<<(tdir[j]+j*4);
                    if(((tused[ty*W+tx]>>(j*4))&(1<<nxd))==0){
                        ++tsc;
                        tdir[j]=nxd;
                        //used[ty][tx]|=(1<<dir[j])<<(j*4);
                        tpy[j]=ty,tpx[j]=tx;
                    }
                    else ng[j]=true;
                }
            }
        if(tsc==0){
            break;
        }
        ret+=tsc;
    }
    return ret;
}
int curid=1;
bool flag=false;
vector<State> pre,nxt;
ll hh[10000];
void improve(){
    int width=450;
    //priority_queue<State> pre;
    ll hs=0;
    rep(i,R) hs^=hh[(i+1)*(py[0][i]*W+px[0][i])];
    pre.pb(State(0,bestscore,0,hs));
    rep(i,R) alv[0].pb(i);
    double tm;
    rep(loop,10000){
        tm=timer.get();
        if(tm>timeLimit){
            cerr<<loop<<endl;
            break;
        }
        nxt.clear();
        int score;
        int sz=pre.size();
        rep(i,width){
            if(sz<=i) break;
            State cur=pre[i];
            int id=cur.id,score=cur.eval,num=cur.cur;
            
            int tsz=alv[id].size();
            int tg=alv[id][num];
            int cx=px[id][tg],cy=py[id][tg],cd=dir[id][tg];
            ll hs=cur.hash^hh[(cy*W+cx)*(tg+1)];
            /*
            int nid=curid++;
            memcpy(grid[nid],grid[id],sizeof(grid[id]));
            memcpy(used[nid],used[id],sizeof(used[id]));
            rep(kk,R) px[nid][kk]=px[id][kk],py[nid][kk]=py[id][kk],dir[nid][kk]=dir[id][kk];
            if(curid==SZ) curid=0;
            */
            if(grid[id][cy*W+cx]>0||used[id][cy*W+cx]>0){
                int nid=curid++;
                memcpy(grid[nid],grid[id],sizeof(grid[id]));
                memcpy(used[nid],used[id],sizeof(used[id]));
                rep(kk,R) px[nid][kk]=px[id][kk],py[nid][kk]=py[id][kk],dir[nid][kk]=dir[id][kk];
                if(curid==SZ) curid=0;
                int tx=(cx+dx[cd]+W)%W,ty=(cy+dy[cd]+H)%H;
                int nxd=grid[id][ty*W+tx]>0?grid[id][ty*W+tx]%10-1:cd;
                if(grid[id][ty*W+tx]==-1||((used[id][ty*W+tx]>>(tg*4))&(1<<nxd))){
                    if(tsz==1) continue;
                    alv[nid]=alv[id];
                    alv[nid].erase(alv[nid].begin()+num);
                    nxt.pb(State(nid,score,num%(tsz-1),hs));
                }
                else{
                    used[nid][cy*W+cx]|=used[id][cy*W+cx]|(1ll<<(tg*4+cd));
                    
                    px[nid][tg]=tx,py[nid][tg]=ty,dir[nid][tg]=nxd;
                    alv[nid]=alv[id];
                    nxt.pb(State(nid,score,(num+1)%tsz,hs^hh[(tg+1)*(ty*W+tx)]));
                }
            }
            else{
                rep(k,4){
                    if(((cd+2)&3)==k) continue;
                    int nid=curid++;
                    memcpy(grid[nid],grid[id],sizeof(grid[id]));
                    memcpy(used[nid],used[id],sizeof(used[id]));
                    rep(kk,R) px[nid][kk]=px[id][kk],py[nid][kk]=py[id][kk],dir[nid][kk]=dir[id][kk];
                    if(curid==SZ) curid=0;
                    int tx=(cx+dx[k]+W)%W,ty=(cy+dy[k]+H)%H;
                    int nxd=grid[id][ty*W+tx]>0?grid[id][ty*W+tx]%10-1:k;
                    if(grid[id][ty*W+tx]==-1||((used[id][ty*W+tx]>>(tg*4))&(1<<nxd))){
                        if(tsz==1) continue;
                        alv[nid]=alv[id];
                        alv[nid].erase(alv[nid].begin()+num);
                        nxt.pb(State(nid,score,num%(tsz-1),hs));
                    }
                    else{
                        used[nid][cy*W+cx]|=used[id][cy*W+cx]|(1ll<<(tg*4+k));
                    
                        px[nid][tg]=tx,py[nid][tg]=ty,dir[nid][tg]=nxd;
                        alv[nid]=alv[id];
                        if(dir[id][tg]!=k){
                            grid[nid][cy*W+cx]=11+k;
                            int tmpscore=bfs(glp,nid);
                            nxt.pb(State(nid,tmpscore+loop,(num+1)%tsz,hs^hh[(tg+1)*(ty*W+tx)]^hh[(11+k)*(cy*W+cx)]));
                            if(bestscore<tmpscore+loop){
                                bestscore=tmpscore+loop;
                                memcpy(bestgrid,tmpgrid,sizeof(tmpgrid));
                            }
                        }
                        else{
                            px[nid][tg]=tx,py[nid][tg]=ty,dir[nid][tg]=nxd;
                            nxt.pb(State(nid,score,(num+1)%tsz,hs^hh[(tg+1)*(ty*W+tx)]));
                        }
                    }
                }
            }
        }
        if(nxt.empty()) break;
        sort(nxt.begin(),nxt.end());
        nxt.erase(unique(nxt.begin(),nxt.end()),nxt.end());
        swap(pre,nxt);
    }


    return;
}
int main(){
    
    string ts;
    rep(i,H){
        cin>>ts;
        rep(j,ts.size()){
            if(ts[j]=='#') grid[0][i*W+j]=-1;
            else if(ts[j]=='U') grid[0][i*W+j]=1;
            else if(ts[j]=='R') grid[0][i*W+j]=2;
            else if(ts[j]=='D') grid[0][i*W+j]=3;
            else if(ts[j]=='L') grid[0][i*W+j]=4;
        }
    }
    cin>>R;
    string ttd;
    rep(i,R){
        cin>>px[0][i]>>py[0][i]>>ttd;
        if(grid[0][py[0][i]*W+px[0][i]]==0){
            if(ttd=="U") dir[0][i]=0;
            else if(ttd=="R") dir[0][i]=1;
            else if(ttd=="D") dir[0][i]=2;
            else dir[0][i]=3;
        }
        else dir[0][i]=grid[0][py[0][i]*W+px[0][i]]-1;
        //used[py[i]][px[i]]=(1<<dir[i])<<(i*4);
    }
    timer.reset();
    rep(i,50*190) hh[i]=rnd.nextInt();
    bestscore=bfs(true,0);
    memcpy(bestgrid,tmpgrid,sizeof(tmpgrid));

    cerr<<"initscore: "<<bestscore<<endl;
    improve();
    cerr<<"finalscore: "<<bestscore<<endl;
    rep(i,H)rep(j,W)if(bestgrid[i*W+j]>=11){
        cout<<j<<" "<<i<<" "<<direction[bestgrid[i*W+j]-11]<<" ";
    }
    cout<<endl;
    // Write an action using cout. DON'T FORGET THE "<< endl"
    // To debug: cerr << "Debug messages..." << endl;
    //cout << "0 0 U 1 1 R 2 2 D 3 3 L" << endl;
    return 0;
}