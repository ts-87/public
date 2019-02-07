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
const string direction="URDL";
const int dx[]={0,1,0,-1},dy[]={-1,0,1,0};
//int8_t grid[10][19],bestgrid[10][19],initgrid[10][19],tmpgrid[10][19],basegrid[10][19];
int8_t grid[190],bestgrid[190];
ll used[190],bestused[190],tused[190];
//ll used[10][19],bestused[10][19],baseused[10][19];
int R;
int px[10],py[10],dir[10];
bool ng[10];
int bestscore=0;


int init(){
    
    int ret=0;
    int tpx[10],tpy[10],tdir[10];
    rep(i,R) tpx[i]=px[i],tpy[i]=py[i],tdir[i]=dir[i];
    while(1){

        int tsc=0;

            rep(j,R)if(!ng[j]){
                int td=tdir[j];
                int tx=(tpx[j]+dx[td]+W)%W,ty=(tpy[j]+dy[td]+H)%H;
                int nxd=grid[ty*W+tx]>0?grid[ty*W+tx]%10-1:td;
                if(grid[ty*W+tx]==-1||((used[ty*W+tx]>>(j*4))&(1<<nxd))){
                    if(grid[tpy[j]*W+tpx[j]]!=0||used[tpy[j]*W+tpx[j]]!=0){
                        ng[j]=true;
                        continue;
                    }
                    rep(kk,4){
                        int k;
                        k=(td+kk+3)&3;
                        if(k!=td){
                        int ttx=(tpx[j]+dx[k]+W)%W,tty=(tpy[j]+dy[k]+H)%H;
                        nxd=grid[tty*W+ttx]>0?grid[tty*W+ttx]%10-1:k;
                        if(grid[tty*W+ttx]!=-1&&((used[tty*W+ttx]>>(j*4))&(1<<nxd))==0){
                            
                            ++tsc;
                            grid[tpy[j]*W+tpx[j]]=11+k;
                            //used[tty][ttx]|=(1<<dir[j])<<(j*4);
                            used[tpy[j]*W+tpx[j]]|=1ll<<(k+j*4);
                            tdir[j]=nxd;
                            tpy[j]=tty,tpx[j]=ttx;
                            break;
                        }
                        }
                        if(kk==3)ng[j]=true;
                    }
                }
                else{
                    
                    if(grid[tpy[j]*W+tpx[j]]>0) tdir[j]=grid[tpy[j]*W+tpx[j]]%10-1;
                    used[tpy[j]*W+tpx[j]]|=1ll<<(tdir[j]+j*4);
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
vector<int> dirset,empset,bestdir,bestemp;
int8_t visit[10][190];
int getscore(){
    int ret=0;
    int tx,ty,tdir;
    memset(visit,0,sizeof(visit));
    rep(i,R){
        tx=px[i],ty=py[i],tdir=grid[ty*W+tx]==0?dir[i]:grid[ty*W+tx]%10-1;
        while(1){
            
            if(grid[ty*W+tx]==-1||(visit[i][ty*W+tx]>>tdir&1)) break;
            visit[i][ty*W+tx]|=(1<<tdir);
            tx=(tx+dx[tdir]+W)%W,ty=(ty+dy[tdir]+H)%H;
            ++ret;
            tdir=grid[ty*W+tx]==0?tdir:grid[ty*W+tx]%10-1;
        }
    }
    return ret;
}
inline bool forceupdate(double sub,double temp){
    if(sub>=0) return true;
    double d=(sub)*temp;
    if(d<-6) return false;
    return exp(d)>rnd.nextDouble();
}
vector<int> cand;

void improve(){
    int cur_score=bestscore;
    double starttemp=20,endtemp=0.1,curtemp;
    int turn=0,upturn=0;
    double invtl=1.0/timeLimit;
    while(1){
        ++turn;
        double t=timer.get();
        if(t>timeLimit){
            cerr<<turn<<endl;
            return;
        }
        if(turn-upturn>100){
            upturn=turn;
            cur_score=bestscore;
            dirset=bestdir;
            empset=bestemp;
            memcpy(grid,bestgrid,sizeof(bestgrid));
        }
        int pre_score=cur_score;
        curtemp=1.0/(starttemp+(endtemp-starttemp)*t*invtl);
        if(!empset.empty()&&((rnd.nextInt()&1)||dirset.empty())){
            int sz=empset.size();
            int id=rnd.nextInt(sz);
            cand.clear();
            int cx=empset[id]%W,cy=empset[id]/W;
            rep(k,4){
                int tx=(cx+dx[k]+W)%W,ty=(cy+dy[k]+H)%H;
                if(grid[ty*W+tx]>=0) cand.pb(k);
            }
            if(cand.size()==0) continue;
            int nxd=rnd.nextInt(cand.size());
            grid[empset[id]]=11+cand[nxd];
            cur_score=getscore();
            if(forceupdate(cur_score-pre_score,curtemp)){
                pre_score=cur_score;
                dirset.pb(empset[id]);
                swap(empset[id],empset[sz-1]);
                empset.pop_back();
                if(bestscore<cur_score){
                    bestscore=cur_score;
                    bestdir=dirset;
                    bestemp=empset;
                    upturn=turn;
                    memcpy(bestgrid,grid,sizeof(grid));
                }
            }
            else{
                grid[empset[id]]=0;
            }
        }
        else{
            int sz=dirset.size();
            int id=rnd.nextInt(sz);
            int predata=grid[dirset[id]];
            grid[dirset[id]]=0;
            cur_score=getscore();
            if(forceupdate(cur_score-pre_score,curtemp)){
                pre_score=cur_score;
                empset.pb(dirset[id]);
                swap(dirset[id],dirset[sz-1]);
                dirset.pop_back();
                if(bestscore<cur_score){
                    bestscore=cur_score;
                    bestdir=dirset;
                    bestemp=empset;
                    upturn=turn;
                    memcpy(bestgrid,grid,sizeof(grid));
                }
            }
            else{
                grid[dirset[id]]=predata;
            }
        }
    }
    return;
}
int main(){
    
    string ts;
    rep(i,H){
        cin>>ts;
        rep(j,ts.size()){
            if(ts[j]=='#') grid[i*W+j]=-1;
            else if(ts[j]=='U') grid[i*W+j]=1;
            else if(ts[j]=='R') grid[i*W+j]=2;
            else if(ts[j]=='D') grid[i*W+j]=3;
            else if(ts[j]=='L') grid[i*W+j]=4;
        }
    }
    cin>>R;
    string ttd;
    rep(i,R){
        cin>>px[i]>>py[i]>>ttd;
        if(grid[py[i]*W+px[i]]==0){
            if(ttd=="U") dir[i]=0;
            else if(ttd=="R") dir[i]=1;
            else if(ttd=="D") dir[i]=2;
            else dir[i]=3;
        }
        else dir[i]=grid[py[i]*W+px[i]]-1;
        //used[py[i]][px[i]]=(1<<dir[i])<<(i*4);
    }
    timer.reset();
    bestscore=init();
    memcpy(bestgrid,grid,sizeof(grid));

    rep(i,190){
        if(grid[i]==0) empset.pb(i);
        else if(grid[i]>10) dirset.pb(i);
    }
    bestdir=dirset;
    bestemp=empset;
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