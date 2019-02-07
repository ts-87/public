#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

double timeLimit=9.92;
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

int H,W;
const int LOOPNUM=500000;
int8_t degP[100][100];
bool degK[100][100];
const int dx[]={-2,-2,-1,-1,1,1,2,2},dy[]={-1,1,-2,2,2,-2,1,-1};
const int dx2[]={0,3,1,1,2,2},dy2[]={0,0,1,2,1,2};
int bestscore=0;
const int MEMOSZ=2000;
vector<int> route[MEMOSZ],ng[MEMOSZ],bestroute;
vector<string> BOARD;
bool ret[100][100],bestans[100][100];
struct State{
    int eval,x,y,routeid,length,px,py;
    State(){};
    bool operator<(const State &r)const{return eval<r.eval;}
};
int curId;
pint psmemo[MEMOSZ];

bool memo[4][100][100];
bool memodegK[4][100][100];
int memoscore[4];

int Bsearch(int y,int x,bool ret[100][100],int depth){
    State initState;
    initState.x=x,initState.y=y,initState.routeid=curId,initState.length=1;
    route[curId].clear();
    route[curId].pb(x+y*W);
    ++curId;
    vector<State> curStates,nextStates;
    curStates.pb(initState);
    int beamwidth=20,turn=100;
    State beststate=initState;
    rep(i,turn){
        nextStates.clear();
        int width=min(beamwidth,(int)curStates.size());
        rep(j,width){
            int x=curStates[j].x,y=curStates[j].y;
            rep(k,8){
                int tx=x+dx[k],ty=y+dy[k];
                if(tx<0||tx>=W||ty<0||ty>=H) continue;
                if(degK[ty][tx]||degP[ty][tx]>1) continue;
                //if(i==0&&degP[ty][tx]==1) continue;
                if(BOARD[ty][tx]=='P'||ret[ty][tx]) continue;
                int tmpid=curStates[j].routeid;
                int sz=route[tmpid].size();
                if(find(route[tmpid].begin(),route[tmpid].begin()+sz-1,tx+ty*W)!=route[tmpid].begin()+sz-1) continue;
                if(find(ng[tmpid].begin(),ng[tmpid].end(),tx+ty*W)!=ng[tmpid].end()) continue;
                if(degP[ty][tx]==1&&beststate.length<route[tmpid].size()+1){
                    curStates[j].length=route[tmpid].size()+1;
                    beststate=curStates[j];
                    bestroute=route[tmpid];
                    bestroute.pb(tx+ty*W);
                    if(turn==100)turn=curStates[j].length+depth;
                }
                else if(degP[ty][tx]==0){
                    State tmp=curStates[j];
                    int cnt=0;
                    /*
                    rep(a,8){
                        int ttx=tx+dx[a],tty=ty+dy[a];
                        if(ttx>=0&&ttx<W&&tty>=0&&tty<H&&degK[tty][ttx]==0&&degP[tty][ttx]==0) ++cnt;
                    }
                    */
                    tmp.eval=cnt;
                    psmemo[curId]={tmp.routeid,tx+ty*W};
                    tmp.routeid=curId;
                    tmp.px=tmp.x,tmp.py=tmp.y;
                    tmp.x=tx,tmp.y=ty;
                    if(++curId==MEMOSZ) curId=0;
                    nextStates.pb(tmp);
                }
            }
        }
        if(nextStates.size()==0) return beststate.routeid;
        int sz=nextStates.size();
        swap(curStates,nextStates);
        if(sz>beamwidth)rep(i,sz){
            swap(curStates[rnd.nextInt(sz)],curStates[rnd.nextInt(sz)]);
        }
        rep(j,min(beamwidth,(int)curStates.size())){
            int tid=curStates[j].routeid;
            route[tid]=route[psmemo[tid].first];
            route[tid].pb(psmemo[tid].second);
            ng[tid]=ng[psmemo[tid].first];
            rep(k,8){
                int tx=curStates[j].px+dx[k],ty=curStates[j].py+dy[k];
                if(tx>=0&&tx<W&&ty>=0&&ty<H&&BOARD[ty][tx]=='.')ng[tid].pb(tx+ty*W);
            }
        }
    }
    return beststate.routeid;
}
bool base[100][100],used[100];
vector<pint> pawn;
class KnightsAndPawns {
public:
    vector<string> placeKnights(vector<string> board) {
        Timer timer;
        BOARD=board;
        H=board.size(),W=board[0].size();
        rep(i,H)rep(j,W){
            if(board[i][j]=='P'){
                rep(k,8){
                    int ty=i+dy[k],tx=j+dx[k];
                    if(tx>=0&&tx<W&&ty>=0&&ty<H&&board[ty][tx]=='.')++degP[ty][tx];
                }
                pawn.pb(i,j);
            }
        }
        int score=0,num_pawn=pawn.size();
        rep(loop,LOOPNUM){
            if(timer.get()>timeLimit) break;
            memcpy(ret,base,sizeof(base));
            memset(degK,0,sizeof(degK));
            score=0;
            if(!(loop&1)){
                int sft=rnd.nextInt(2);
                int s=sft;
                int id=(sft<<1)+(loop&1);
            if(!used[id]){
                for(int x=s;x+3<W;x+=5){
                    int st=0,mx=-1,y=0;
                
                    while(st+3<H){
                        bool flag=true;
                        rep(k,6){
                            int tx=x+dx2[k],ty=y+dy2[k];
                            if(degK[ty][tx]||(degP[ty][tx]!=0&&board[ty][tx]=='.')){
                                flag=false;break;
                            }
                        }
                        if(!flag){
                            ++st,y=st;
                            continue;
                        }
                        else{
                            y+=3;
                        }
                
                        while(y<H){
                    
                            if((board[y][x]=='P'||(!degK[y][x]&&degP[y][x]==0))&&(board[y][x+3]=='P'||(!degK[y][x+3]&&degP[y][x+3]==0))){
                                mx=y;
                            }
                            if((board[y][x+1]=='.'&&(degK[y][x+1]||degP[y][x+1]!=0))||(board[y][x+2]=='.'&&(degK[y][x+2]||degP[y][x+2]!=0))
                            ||board[y][x]=='P'||board[y][x+3]=='P'){
                                FOR(yy,st,mx+1){
                                    if(yy==st||yy==mx){
                                        int ty=yy,tx=x;
                                        if(board[ty][tx]=='.'){
                                            ++score,ret[ty][tx]=true;
                                            rep(j,8){
                                                int tty=ty+dy[j],ttx=tx+dx[j];
                                                if(ttx>=0&&ttx<W&&tty>=0&&tty<H&&board[tty][ttx]=='.')degK[tty][ttx]=true;
                                            }
                                        }
                                        ty=yy,tx=x+3;
                                        if(board[ty][tx]=='.'){
                                            ++score,ret[ty][tx]=true;
                                            rep(j,8){
                                                int tty=ty+dy[j],ttx=tx+dx[j];
                                                if(ttx>=0&&ttx<W&&tty>=0&&tty<H&&board[tty][ttx]=='.')degK[tty][ttx]=true;
                                            }
                                        }
                                    }
                                    else{
                                        int ty=yy,tx=x+1;
                                        if(board[ty][tx]=='.'){
                                            ++score,ret[ty][tx]=true;
                                            rep(j,8){
                                                int tty=ty+dy[j],ttx=tx+dx[j];
                                                if(ttx>=0&&ttx<W&&tty>=0&&tty<H&&board[tty][ttx]=='.')degK[tty][ttx]=true;
                                            }
                                        }
                                        ty=yy,tx=x+2;
                                        if(board[ty][tx]=='.'){
                                            ++score,ret[ty][tx]=true;
                                            rep(j,8){
                                                int tty=ty+dy[j],ttx=tx+dx[j];
                                                if(ttx>=0&&ttx<W&&tty>=0&&tty<H&&board[tty][ttx]=='.')degK[tty][ttx]=true;
                                            }
                                        }
                                    }
                                }
                                st=y+1;
                                ++y;
                                mx=-1;
                                break;
                            }
                            else ++y;
                        }
                        if(mx!=-1){
                            FOR(yy,st,mx+1){
                                if(yy==st||yy==mx){
                                    int ty=yy,tx=x;
                                    if(board[ty][tx]=='.'){
                                        ++score,ret[ty][tx]=true;
                                        rep(j,8){
                                            int tty=ty+dy[j],ttx=tx+dx[j];
                                            if(ttx>=0&&ttx<W&&tty>=0&&tty<H&&board[tty][ttx]=='.')degK[tty][ttx]=true;
                                        }
                                    }
                                    ty=yy,tx=x+3;
                                    if(board[ty][tx]=='.'){
                                        ++score,ret[ty][tx]=true;
                                        rep(j,8){
                                            int tty=ty+dy[j],ttx=tx+dx[j];
                                            if(ttx>=0&&ttx<W&&tty>=0&&tty<H&&board[tty][ttx]=='.')degK[tty][ttx]=true;
                                        }
                                    }
                                }
                                else{
                                    int ty=yy,tx=x+1;
                                    if(board[ty][tx]=='.'){
                                        ++score,ret[ty][tx]=true;
                                        rep(j,8){
                                            int tty=ty+dy[j],ttx=tx+dx[j];
                                            if(ttx>=0&&ttx<W&&tty>=0&&tty<H&&board[tty][ttx]=='.')degK[tty][ttx]=true;
                                        }
                                    }
                                    ty=yy,tx=x+2;
                                    if(board[ty][tx]=='.'){
                                        ++score,ret[ty][tx]=true;
                                        rep(j,8){
                                            int tty=ty+dy[j],ttx=tx+dx[j];
                                            if(ttx>=0&&ttx<W&&tty>=0&&tty<H&&board[tty][ttx]=='.')degK[tty][ttx]=true;
                                        }
                                    }
                                }
                            }
                            
                        }
                        st=y+1;
                        ++y;
                        mx=-1;
                    }
                }    
                memcpy(memo[id],ret,sizeof(ret));
                memcpy(memodegK[id],degK,sizeof(degK));
                memoscore[id]=score;
                used[id]=true;
                }
                else{
                    memcpy(ret,memo[id],sizeof(memo[id]));
                    memcpy(degK,memodegK[id],sizeof(memodegK[id]));
                    score=memoscore[id];
                }
            }
            
            else if(loop&1){
                int sft=rnd.nextInt(2);
                int s=sft;
                int id=(sft<<1)+(loop&1);
                if(!used[id]){
                    for(int y=s;y+3<H;y+=5){
                    int st=0,mx=-1,x=0;
                
                while(st+3<W){
                    bool flag=true;
                    rep(k,6){
                        int tx=x+dy2[k],ty=y+dx2[k];
                        if(degK[ty][tx]||(degP[ty][tx]!=0&&board[ty][tx]=='.')){
                            flag=false;break;
                        }
                    }
                    if(!flag){
                        ++st,x=st;
                        continue;
                    }
                    else{
                        x+=3;
                    }
                
                    while(x<W){
                    
                        if((board[y][x]=='P'||(!degK[y][x]&&degP[y][x]==0))&&(board[y+3][x]=='P'||(!degK[y+3][x]&&degP[y+3][x]==0))){
                            mx=x;
                        }
                        if((board[y+1][x]=='.'&&(degK[y+1][x]||degP[y+1][x]!=0))||(board[y+2][x]=='.'&&(degK[y+2][x]||degP[y+2][x]!=0))
                        ||board[y][x]=='P'||board[y+3][x]=='P'){
                            FOR(xx,st,mx+1){
                                if(xx==st||xx==mx){
                                    int ty=y,tx=xx;
                                    if(board[ty][tx]=='.'){
                                        ++score,ret[ty][tx]=true;
                                        rep(j,8){
                                            int tty=ty+dy[j],ttx=tx+dx[j];
                                            if(ttx>=0&&ttx<W&&tty>=0&&tty<H&&board[tty][ttx]=='.')degK[tty][ttx]=true;
                                        }
                                    }
                                    ty=y+3,tx=xx;
                                    if(board[ty][tx]=='.'){
                                        ++score,ret[ty][tx]=true;
                                        rep(j,8){
                                            int tty=ty+dy[j],ttx=tx+dx[j];
                                            if(ttx>=0&&ttx<W&&tty>=0&&tty<H&&board[tty][ttx]=='.')degK[tty][ttx]=true;
                                        }
                                    }
                                }
                                else{
                                    int ty=y+1,tx=xx;
                                    if(board[ty][tx]=='.'){
                                        ++score,ret[ty][tx]=true;
                                        rep(j,8){
                                            int tty=ty+dy[j],ttx=tx+dx[j];
                                            if(ttx>=0&&ttx<W&&tty>=0&&tty<H&&board[tty][ttx]=='.')degK[tty][ttx]=true;
                                        }
                                    }
                                    ty=y+2,tx=xx;
                                    if(board[ty][tx]=='.'){
                                        ++score,ret[ty][tx]=true;
                                        rep(j,8){
                                            int tty=ty+dy[j],ttx=tx+dx[j];
                                            if(ttx>=0&&ttx<W&&tty>=0&&tty<H&&board[tty][ttx]=='.')degK[tty][ttx]=true;
                                        }
                                    }
                                }
                            }
                            st=x+1;
                            ++x;
                            mx=-1;
                            break;
                        }
                        else ++x;
                    }
                    if(mx!=-1){
                        FOR(xx,st,mx+1){
                            if(xx==st||xx==mx){
                                int ty=y,tx=xx;
                                if(board[ty][tx]=='.'){
                                    ++score,ret[ty][tx]=true;
                                    rep(j,8){
                                        int tty=ty+dy[j],ttx=tx+dx[j];
                                        if(ttx>=0&&ttx<W&&tty>=0&&tty<H&&board[tty][ttx]=='.')degK[tty][ttx]=true;
                                    }
                                }
                                ty=y+3,tx=xx;
                                if(board[ty][tx]=='.'){
                                    ++score,ret[ty][tx]=true;
                                    rep(j,8){
                                        int tty=ty+dy[j],ttx=tx+dx[j];
                                        if(ttx>=0&&ttx<W&&tty>=0&&tty<H&&board[tty][ttx]=='.')degK[tty][ttx]=true;
                                    }
                                }
                            }
                            else{
                                int ty=y+1,tx=xx;
                                if(board[ty][tx]=='.'){
                                    ++score,ret[ty][tx]=true;
                                    rep(j,8){
                                        int tty=ty+dy[j],ttx=tx+dx[j];
                                        if(ttx>=0&&ttx<W&&tty>=0&&tty<H&&board[tty][ttx]=='.')degK[tty][ttx]=true;
                                    }
                                }
                                ty=y+2,tx=xx;
                                if(board[ty][tx]=='.'){
                                    ++score,ret[ty][tx]=true;
                                    rep(j,8){
                                        int tty=ty+dy[j],ttx=tx+dx[j];
                                        if(ttx>=0&&ttx<W&&tty>=0&&tty<H&&board[tty][ttx]=='.')degK[tty][ttx]=true;
                                    }
                                }
                            }
                        }
                        
                    }
                    st=x+1;
                    ++x;
                    mx=-1;
                }
                    }    
                memcpy(memo[id],ret,sizeof(ret));
                memcpy(memodegK[id],degK,sizeof(degK));
                memoscore[id]=score;
                used[id]=true;
                }
                else{
                    memcpy(ret,memo[id],sizeof(memo[id]));
                    memcpy(degK,memodegK[id],sizeof(memodegK[id]));
                    score=memoscore[id];
                }
            }
            

        if(num_pawn!=0){
        int st=rnd.nextInt(num_pawn);
        int depth=rnd.nextInt(15)+2;
        //rep(i,H)rep(j,W)if(board[i][j]=='P'){
        //rep(i,num_pawn){
        for(int i=st,cnt=0;cnt<num_pawn;i=(i+1)%num_pawn,++cnt){
            
            rep(a,8){
                int b=(a+loop/2)%8;
                int ty=pawn[i].first+dy[b],tx=pawn[i].second+dx[b];
                if(tx>=0&&tx<W&&ty>=0&&ty<H&&!degK[ty][tx]&&degP[ty][tx]==1&&board[ty][tx]=='.'&&!ret[ty][tx]){
                    curId=0;
                    bestroute.clear();
                    Bsearch(ty,tx,ret,depth);
                    int sz=bestroute.size();
                    rep(k,sz){
                        int yy=bestroute[k]/W,xx=bestroute[k]%W;
                        ret[yy][xx]=true;
                        ++score;
                        rep(l,8){
                            int tty=yy+dy[l],ttx=xx+dx[l];
                            if(!degK[tty][ttx]&&ttx>=0&&ttx<W&&tty>=0&&tty<H&&board[tty][ttx]=='.')degK[tty][ttx]=true;
                        }
                    }
                }
            }
        }
        rep(i,H)rep(j,W)if(board[i][j]=='.'&&!ret[i][j]){
            if(degP[i][j]==2&&degK[i][j]==0){
                ++score,ret[i][j]=true;
                rep(k,8){
                    int tty=i+dy[k],ttx=j+dx[k];
                    if(!degK[tty][ttx]&&ttx>=0&&ttx<W&&tty>=0&&tty<H&&board[tty][ttx]=='.')degK[tty][ttx]=true;
                }
            }
        }
        }
        if(bestscore<score) bestscore=score,memcpy(bestans,ret,sizeof(ret));
        }
        //cerr<<bestscore<<" "<<timer.get()<<endl;
        vector<string> ans=vector<string>(H,string(W,'.'));
        rep(i,H)rep(j,W)if(bestans[i][j]) ans[i][j]='K';
        return ans;
    }
};

// -------8<------- end of solution submitted to the website -------8<-------

template<class T> void getVector(vector<T>& v) {
    for (int i = 0; i < v.size(); ++i)
        cin >> v[i];
}

int main() {
    KnightsAndPawns kap;
    int H;
    cin >> H;
    vector<string> board(H);
    getVector(board);
    
    vector<string> ret = kap.placeKnights(board);
    cout << ret.size() << endl;
    for (int i = 0; i < (int)ret.size(); ++i)
        cout << ret[i] << endl;
    cout.flush();
}
