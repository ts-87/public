#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

double timeLimit=9.9,timeLimit2=3.0;
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
        //y=random_device()();
	}
	uint32_t nextInt() {
    	return y^=(y^=(y^=y<<13)>>17)<<5;
	}
	uint32_t nextInt(uint32_t n) {
		return nextInt()%n;
	}
	double nextDouble() {return nextInt()*iDouble;}
} rnd;

struct vset{
    vector<int> st,pos;
    vset(){
    }
    vset(int n){
        pos.resize(n,-1);
        st.reserve(n);
    }
    inline void resize(int n){
        pos.resize(n,-1);
        st.reserve(n);
    }
    inline void insert(int a){
        if(pos[a]==-1){
            pos[a]=st.size();
            st.push_back(a);
        }
    }
    inline void erase(int a){
        if(pos[a]!=-1){
            swap(st[pos[a]],st.back());
            pos[st[pos[a]]]=pos[a];
            st.pop_back();
            pos[a]=-1;
        }
    }
    inline int size(){
        return st.size();
    }
    inline void clear(){
        st.clear();
        fill(pos.begin(),pos.end(),-1);
    }
};

struct BIT2d{
    int n,m;
    int dat[51][51];
    void init(int x,int y){
        n=x,m=y;
        memset(dat,0,sizeof(dat));
    }
    inline void clear(){
        memset(dat,0,sizeof(dat));
    }
    inline void add(int x,int y,int k){
        for(int i=x;i<=n;i+=i&-i)for(int j=y;j<=m;j+=j&-j) dat[i][j]+=k;
    }
    inline int sum(int x,int y){
        int s=0;
        for(int i=x;i>0;i-=i&-i)for(int j=y;j>0;j-=j&-j) s+=dat[i][j];
        return s;
    }
    inline int sum(int lx,int ly,int rx,int ry){
        return sum(rx,ry)-sum(lx-1,ry)-sum(rx,ly-1)+sum(lx-1,ly-1);
    }
};

vset black,white;
BIT2d ftr;
int parea[51][51];
int pval[2505],pvalp[51][51],pvalm[51][51];
double LN[65536];
int TRYNM=4;
bool used[51][51];
int vg[51][51]; 
class PointsOnGrid {
public:
    Timer timer;
    int H,W,pH,pW;
    int totpena=0;
    int Kmn,Kmx;
    //vset black,white;
    int grid[2501];
    
    int bestans[51][51],bestscore=-1,initscore;
    vector<int> bestans2;
    vector<pint> sortnum;
    int bpena=0;
    int sflnum=50;

    inline bool forceupdate(double diff,double temp){
        if(diff>=0) return true;
        return diff*temp>LN[rnd.nextInt()&65535];
        //double d=(diff)*temp;
        //if(d<-6) return false;
        //return exp(d)>rnd.nextDouble();
    }
    
    void improve_special(){
        double start=timer.get();
        int turn=0;
        double pf=10,diff;
        double t=start,invtl=1.0/timeLimit*TRYNM;
        int curpena=bpena;
        int curscore=initscore;
        double starttemp=3,endtemp=0.1,ts=starttemp;
        double limit=(timeLimit-start)/TRYNM;
        //double PI=acos(-1.0);
        rep(kr,H)rep(kc,W){
            if(parea[kr][kc])pvalm[kr][kc]=pval[parea[kr][kc]-1]-pval[parea[kr][kc]];
            pvalp[kr][kc]=pval[parea[kr][kc]+1]-pval[parea[kr][kc]];
        }
        while(1){
            ++turn;
            if((turn&15)==0){
                t=timer.get();
                if(t>start+limit) start=t,starttemp-=0.4;
                //ts=endtemp+0.5*(starttemp-endtemp)*(1.0+cos((t-start)*invtl*PI));
                ts=(starttemp+(endtemp-starttemp)*(t-start)*invtl);
                //pf=(spf+(epf-spf)*(t-start))*ts;
            }
            if(t>timeLimit){
                cerr<<turn<<endl;
                return;
            }
            //ts=(starttemp+(endtemp-starttemp)*(t-start)*invtl);
            //ts=endtemp+0.5*(starttemp-endtemp)*(1.0+cos((t-start)*invtl*PI));
            //curtemp=1.0/ts;
            //pf=spf+(epf-spf)*(t-start);
            int sel=rnd.nextInt()&7;
            int nxscore=curscore,nxpena=curpena;
            if(black.size()==0) sel=1;
            else if(white.size()==0) sel=0;
            int cnt=3,nx;
            if(sel<1){
                int pos;
                if(turn&3){
                    while(pos=black.st[rnd.nextInt(black.size())],grid[pos]>=10-cnt)--cnt;
                }
                else pos=black.st[rnd.nextInt(black.size())];
                int py=pos/W,px=pos%W;
                nxscore-=grid[pos];
                FOR(kr,max(0,py-pH+1),min(py+1,H-pH+1)){
                    FOR(kc,max(0,px-pW+1),min(px+1,W-pW+1)){
                        nxpena+=pvalm[kr][kc];
                    }
                }
                //if(nxscore-pf*nxpena>=curscore-pf*curpena){
                diff=nxscore-pf*nxpena*nxpena-curscore+pf*curpena*curpena;
                //diff=nxscore-pf*nxpena-curscore+pf*curpena;
                if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
                    curscore=nxscore;
                    curpena=nxpena;
                    FOR(kr,max(0,py-pH+1),min(py+1,H-pH+1)){
                        FOR(kc,max(0,px-pW+1),min(px+1,W-pW+1)){;
                            nx=--parea[kr][kc];
                            if(nx)pvalm[kr][kc]=pval[nx-1]-pval[nx];
                            pvalp[kr][kc]=pval[nx+1]-pval[nx];
                        }
                    }
                    black.erase(pos);
                    white.insert(pos);
                    if(nxpena==0&&nxscore>bestscore){
                        bestscore=nxscore;
                        bestans2=black.pos;
                        //cerr<<timer.get()<<" "<<sel<<" "<<bestscore<<endl;
                    }
                }
            }
            else if(sel<2){
                int pos;
                if(turn&3){
                    while(pos=white.st[rnd.nextInt(white.size())],grid[pos]<cnt)--cnt;
                }
                else pos=white.st[rnd.nextInt(white.size())];
                int py=pos/W,px=pos%W;
                nxscore+=grid[pos];
                FOR(kr,max(0,py-pH+1),min(py+1,H-pH+1)){
                    FOR(kc,max(0,px-pW+1),min(px+1,W-pW+1)){
                        nxpena+=pvalp[kr][kc];
                    }
                }
                //if(nxscore-pf*nxpena>=curscore-pf*curpena){
                diff=nxscore-pf*nxpena*nxpena-curscore+pf*curpena*curpena;
                //diff=nxscore-pf*nxpena-curscore+pf*curpena;
                if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
                    curscore=nxscore;
                    curpena=nxpena;
                    FOR(kr,max(0,py-pH+1),min(py+1,H-pH+1)){
                        FOR(kc,max(0,px-pW+1),min(px+1,W-pW+1)){;
                            nx=++parea[kr][kc];
                            pvalm[kr][kc]=pval[nx-1]-pval[nx];
                            pvalp[kr][kc]=pval[nx+1]-pval[nx];
                        }
                    }
                    white.erase(pos);
                    black.insert(pos);
                    if(nxpena==0&&nxscore>bestscore){
                        bestscore=nxscore;
                        bestans2=black.pos;
                        //cerr<<timer.get()<<" "<<sel<<" "<<bestscore<<endl;
                    }
                }
            }
            else{
                int pos1,pos2,py1,px1,py2,px2;
                /*
                if(turn&3){
                    while(1){
                        pos1=white.st[rnd.nextInt(white.size())];
                        pos2=black.st[rnd.nextInt(black.size())];
                        if(grid[pos1]>=grid[pos2]+cnt) break;
                        else --cnt;
                    }
                }
                else{
                    pos1=white.st[rnd.nextInt(white.size())];
                    pos2=black.st[rnd.nextInt(black.size())];
                }
                */
                    rep(i,10){
                        pos1=white.st[rnd.nextInt(white.size())];
                        pos2=black.st[rnd.nextInt(black.size())];
                        py1=pos1/W,px1=pos1%W,py2=pos2/W,px2=pos2%W;
                        if(abs(py2-py1<pH)&&abs(px2-px1)<pW) break;
                    }
                py1=pos1/W,px1=pos1%W,py2=pos2/W,px2=pos2%W;
                nxscore+=grid[pos1];
                nxscore-=grid[pos2];
                int cl,cr;
                FOR(kr,max(0,py1-pH+1),min(py1+1,H-pH+1)){
                    if(kr>py2||kr+pH-1<py2){
                        cl=max(0,px1-pW+1),cr=min(px1+1,W-pW+1);
                    }
                    else if(px1<=px2){
                        cl=max(0,px1-pW+1),cr=min(px1+1,px2-pW+1);
                    }
                    else{
                        cl=max(px2+1,px1-pW+1),cr=min(px1+1,W-pW+1);
                    }
                    FOR(kc,cl,cr){
                        nxpena+=pvalp[kr][kc];
                    }
                }
                FOR(kr,max(0,py2-pH+1),min(py2+1,H-pH+1)){
                    if(kr>py1||kr+pH-1<py1){
                        cl=max(0,px2-pW+1),cr=min(px2+1,W-pW+1);
                    }
                    else if(px2<=px1){
                        cl=max(0,px2-pW+1),cr=min(px2+1,px1-pW+1);
                    }
                    else{
                        cl=max(px1+1,px2-pW+1),cr=min(px2+1,W-pW+1);
                    }
                    FOR(kc,cl,cr){
                        nxpena+=pvalm[kr][kc];
                    }
                }
                diff=nxscore-pf*nxpena*nxpena-curscore+pf*curpena*curpena;
                //diff=nxscore-pf*nxpena-curscore+pf*curpena;
                if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
                    curscore=nxscore;
                    curpena=nxpena;
                    FOR(kr,max(0,py1-pH+1),min(py1+1,H-pH+1)){
                        if(kr>py2||kr+pH-1<py2){
                            cl=max(0,px1-pW+1),cr=min(px1+1,W-pW+1);
                        }
                        else if(px1<=px2){
                            cl=max(0,px1-pW+1),cr=min(px1+1,px2-pW+1);
                        }
                        else{
                            cl=max(px2+1,px1-pW+1),cr=min(px1+1,W-pW+1);
                        }
                        FOR(kc,cl,cr){
                            nx=++parea[kr][kc];
                            pvalm[kr][kc]=pval[nx-1]-pval[nx];
                            pvalp[kr][kc]=pval[nx+1]-pval[nx];
                        }
                    }
                    FOR(kr,max(0,py2-pH+1),min(py2+1,H-pH+1)){
                        if(kr>py1||kr+pH-1<py1){
                            cl=max(0,px2-pW+1),cr=min(px2+1,W-pW+1);
                        }
                        else if(px2<=px1){
                            cl=max(0,px2-pW+1),cr=min(px2+1,px1-pW+1);
                        }
                        else{
                            cl=max(px1+1,px2-pW+1),cr=min(px2+1,W-pW+1);
                        }
                        FOR(kc,cl,cr){
                            nx=--parea[kr][kc];
                            if(nx)pvalm[kr][kc]=pval[nx-1]-pval[nx];
                            pvalp[kr][kc]=pval[nx+1]-pval[nx];
                        }
                    }
                    white.erase(pos1);
                    white.insert(pos2);
                    black.erase(pos2);
                    black.insert(pos1);
                    //cerr<<timer.get()<<" "<<sel<<" "<<nxpena<<" "<<nxscore<<endl;
                    if(nxpena==0&&nxscore>bestscore){
                        bestscore=nxscore;
                        bestans2=black.pos;
                        //cerr<<timer.get()<<" "<<sel<<" "<<bestscore<<endl;
                    }
                }
            }
        }
    }
    
    void improve(){
        double start=timer.get();
        int turn=0;
        double pf=10,diff;
        double t=start,invtl=1.0/timeLimit;
        int curpena=bpena;
        int curscore=initscore;
        double starttemp=4,endtemp=0.1,ts=starttemp;
        //double limit=(timeLimit-start)/2;
        //double spf=10,epf=5,pf=10;
        double PI=acos(-1.0);
        rep(kr,H)rep(kc,W){
            if(parea[kr][kc])pvalm[kr][kc]=pval[parea[kr][kc]-1]-pval[parea[kr][kc]];
            pvalp[kr][kc]=pval[parea[kr][kc]+1]-pval[parea[kr][kc]];
        }
        while(1){
            ++turn;
            if((turn&15)==0){
                t=timer.get();
                //if(t>start+limit) start=t,starttemp=3;
                ts=endtemp+0.5*(starttemp-endtemp)*(1.0+cos((t-start)*invtl*PI));
                //ts=(starttemp+(endtemp-starttemp)*(t-start)*invtl);
                //pf=spf+(epf-spf)*(t-start);
            }
            if(t>timeLimit){
                cerr<<turn<<endl;
                return;
            }
            //ts=(starttemp+(endtemp-starttemp)*(t-start)*invtl);
            //ts=endtemp+0.5*(starttemp-endtemp)*(1.0+cos((t-start)*invtl*PI));
            //curtemp=1.0/ts;
            //pf=spf+(epf-spf)*(t-start);
            int sel=rnd.nextInt()&7;
            int nxscore=curscore,nxpena=curpena;
            if(black.size()==0) sel=4;
            else if(white.size()==0) sel=0;
            int cnt=3,nx;
            if(sel<3){
                int pos;
                if(turn&3){
                    while(pos=black.st[rnd.nextInt(black.size())],grid[pos]>=10-cnt)--cnt;
                }
                else pos=black.st[rnd.nextInt(black.size())];
                int py=pos/W,px=pos%W;
                nxscore-=grid[pos];
                FOR(kr,max(0,py-pH+1),min(py+1,H-pH+1)){
                    FOR(kc,max(0,px-pW+1),min(px+1,W-pW+1)){
                        nxpena+=pvalm[kr][kc];
                    }
                }
                
                //if(nxscore-pf*nxpena>=curscore-pf*curpena){
                //diff=nxscore-pf*nxpena*nxpena-curscore+pf*curpena*curpena;
                diff=nxscore-pf*nxpena-curscore+pf*curpena;
                if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
                    curscore=nxscore;
                    curpena=nxpena;
                    FOR(kr,max(0,py-pH+1),min(py+1,H-pH+1)){
                        FOR(kc,max(0,px-pW+1),min(px+1,W-pW+1)){;
                            nx=--parea[kr][kc];
                            if(nx)pvalm[kr][kc]=pval[nx-1]-pval[nx];
                            pvalp[kr][kc]=pval[nx+1]-pval[nx];
                        }
                    }
                    black.erase(pos);
                    white.insert(pos);
                    if(nxpena==0&&nxscore>bestscore){
                        bestscore=nxscore;
                        bestans2=black.pos;
                        //cerr<<timer.get()<<" "<<sel<<" "<<bestscore<<endl;
                    }
                }
            }
            else if(sel<6){
                int pos;
                if(turn&3){
                    while(pos=white.st[rnd.nextInt(white.size())],grid[pos]<cnt)--cnt;
                }
                else pos=white.st[rnd.nextInt(white.size())];
                int py=pos/W,px=pos%W;
                nxscore+=grid[pos];
                FOR(kr,max(0,py-pH+1),min(py+1,H-pH+1)){
                    FOR(kc,max(0,px-pW+1),min(px+1,W-pW+1)){
                        nxpena+=pvalp[kr][kc];
                    }
                }
                //if(nxscore-pf*nxpena>=curscore-pf*curpena){
                //diff=nxscore-pf*nxpena*nxpena-curscore+pf*curpena*curpena;
                diff=nxscore-pf*nxpena-curscore+pf*curpena;
                if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
                    curscore=nxscore;
                    curpena=nxpena;
                    FOR(kr,max(0,py-pH+1),min(py+1,H-pH+1)){
                        FOR(kc,max(0,px-pW+1),min(px+1,W-pW+1)){;
                            nx=++parea[kr][kc];
                            pvalm[kr][kc]=pval[nx-1]-pval[nx];
                            pvalp[kr][kc]=pval[nx+1]-pval[nx];
                        }
                    }
                    white.erase(pos);
                    black.insert(pos);
                    if(nxpena==0&&nxscore>bestscore){
                        bestscore=nxscore;
                        bestans2=black.pos;
                        //cerr<<timer.get()<<" "<<sel<<" "<<bestscore<<endl;
                    }
                }
            }
            else{
                int pos1,pos2;
                if(turn&3){
                    while(1){
                        pos1=white.st[rnd.nextInt(white.size())];
                        pos2=black.st[rnd.nextInt(black.size())];
                        if(grid[pos1]>=grid[pos2]+cnt) break;
                        else --cnt;
                    }
                }
                else{
                    pos1=white.st[rnd.nextInt(white.size())];
                    pos2=black.st[rnd.nextInt(black.size())];
                }
                int py1=pos1/W,px1=pos1%W,py2=pos2/W,px2=pos2%W;
                nxscore+=grid[pos1];
                nxscore-=grid[pos2];
                int cl,cr;
                FOR(kr,max(0,py1-pH+1),min(py1+1,H-pH+1)){
                    if(kr>py2||kr+pH-1<py2){
                        cl=max(0,px1-pW+1),cr=min(px1+1,W-pW+1);
                    }
                    else if(px1<=px2){
                        cl=max(0,px1-pW+1),cr=min(px1+1,px2-pW+1);
                    }
                    else{
                        cl=max(px2+1,px1-pW+1),cr=min(px1+1,W-pW+1);
                    }
                    FOR(kc,cl,cr){
                        nxpena+=pvalp[kr][kc];
                    }
                }
                FOR(kr,max(0,py2-pH+1),min(py2+1,H-pH+1)){
                    if(kr>py1||kr+pH-1<py1){
                        cl=max(0,px2-pW+1),cr=min(px2+1,W-pW+1);
                    }
                    else if(px2<=px1){
                        cl=max(0,px2-pW+1),cr=min(px2+1,px1-pW+1);
                    }
                    else{
                        cl=max(px1+1,px2-pW+1),cr=min(px2+1,W-pW+1);
                    }
                    FOR(kc,cl,cr){
                        nxpena+=pvalm[kr][kc];
                    }
                }
                //diff=nxscore-pf*nxpena*nxpena-curscore+pf*curpena*curpena;
                diff=nxscore-pf*nxpena-curscore+pf*curpena;
                if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
                    curscore=nxscore;
                    curpena=nxpena;
                    FOR(kr,max(0,py1-pH+1),min(py1+1,H-pH+1)){
                        if(kr>py2||kr+pH-1<py2){
                            cl=max(0,px1-pW+1),cr=min(px1+1,W-pW+1);
                        }
                        else if(px1<=px2){
                            cl=max(0,px1-pW+1),cr=min(px1+1,px2-pW+1);
                        }
                        else{
                            cl=max(px2+1,px1-pW+1),cr=min(px1+1,W-pW+1);
                        }
                        FOR(kc,cl,cr){
                            nx=++parea[kr][kc];
                            pvalm[kr][kc]=pval[nx-1]-pval[nx];
                            pvalp[kr][kc]=pval[nx+1]-pval[nx];
                        }
                    }
                    FOR(kr,max(0,py2-pH+1),min(py2+1,H-pH+1)){
                        if(kr>py1||kr+pH-1<py1){
                            cl=max(0,px2-pW+1),cr=min(px2+1,W-pW+1);
                        }
                        else if(px2<=px1){
                            cl=max(0,px2-pW+1),cr=min(px2+1,px1-pW+1);
                        }
                        else{
                            cl=max(px1+1,px2-pW+1),cr=min(px2+1,W-pW+1);
                        }
                        FOR(kc,cl,cr){
                            nx=--parea[kr][kc];
                            if(nx)pvalm[kr][kc]=pval[nx-1]-pval[nx];
                            pvalp[kr][kc]=pval[nx+1]-pval[nx];
                        }
                    }
                    white.erase(pos1);
                    white.insert(pos2);
                    black.erase(pos2);
                    black.insert(pos1);
                    //cerr<<timer.get()<<" "<<sel<<" "<<nxpena<<" "<<nxscore<<endl;
                    if(nxpena==0&&nxscore>bestscore){
                        bestscore=nxscore;
                        bestans2=black.pos;
                        //cerr<<timer.get()<<" "<<sel<<" "<<bestscore<<endl;
                    }
                }
            }
        }
    }
    void improve_s(){
        double start=timer.get();
        int turn=0;
        double pf=10,diff;
        double t=start,invtl=1.0/timeLimit*TRYNM;
        int curpena=bpena;
        int curscore=initscore;
        double starttemp=3,endtemp=0.1,ts=starttemp;
        double limit=(timeLimit-start)/TRYNM;
        //double PI=acos(-1.0);
        rep(kr,H)rep(kc,W){
            if(parea[kr][kc])pvalm[kr][kc]=pval[parea[kr][kc]-1]-pval[parea[kr][kc]];
            pvalp[kr][kc]=pval[parea[kr][kc]+1]-pval[parea[kr][kc]];
        }
        while(1){
            ++turn;
            if((turn&15)==0){
                t=timer.get();
                if(t>start+limit) start=t,starttemp-=0.4;
                //ts=endtemp+0.5*(starttemp-endtemp)*(1.0+cos((t-start)*invtl*PI));
                ts=(starttemp+(endtemp-starttemp)*(t-start)*invtl);
                //pf=(spf+(epf-spf)*(t-start))*ts;
            }
            if(t>timeLimit){
                cerr<<turn<<endl;
                return;
            }
            //ts=(starttemp+(endtemp-starttemp)*(t-start)*invtl);
            //ts=endtemp+0.5*(starttemp-endtemp)*(1.0+cos((t-start)*invtl*PI));
            //curtemp=1.0/ts;
            //pf=spf+(epf-spf)*(t-start);
            int sel=rnd.nextInt()&7;
            int nxscore=curscore,nxpena=curpena;
            if(black.size()==0) sel=4;
            else if(white.size()==0) sel=0;
            int cnt=3,nx;
            if(sel<3){
                int pos;
                if(turn&3){
                    while(pos=black.st[rnd.nextInt(black.size())],grid[pos]>=10-cnt)--cnt;
                }
                else pos=black.st[rnd.nextInt(black.size())];
                int py=pos/W,px=pos%W;
                nxscore-=grid[pos];
                FOR(kr,max(0,py-pH+1),min(py+1,H-pH+1)){
                    FOR(kc,max(0,px-pW+1),min(px+1,W-pW+1)){
                        nxpena+=pvalm[kr][kc];
                    }
                }
                //if(nxscore-pf*nxpena>=curscore-pf*curpena){
                diff=nxscore-pf*nxpena*nxpena-curscore+pf*curpena*curpena;
                //diff=nxscore-pf*nxpena-curscore+pf*curpena;
                if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
                    curscore=nxscore;
                    curpena=nxpena;
                    FOR(kr,max(0,py-pH+1),min(py+1,H-pH+1)){
                        FOR(kc,max(0,px-pW+1),min(px+1,W-pW+1)){;
                            nx=--parea[kr][kc];
                            if(nx)pvalm[kr][kc]=pval[nx-1]-pval[nx];
                            pvalp[kr][kc]=pval[nx+1]-pval[nx];
                        }
                    }
                    black.erase(pos);
                    white.insert(pos);
                    if(nxpena==0&&nxscore>bestscore){
                        bestscore=nxscore;
                        bestans2=black.pos;
                        //cerr<<timer.get()<<" "<<sel<<" "<<bestscore<<endl;
                    }
                }
            }
            else if(sel<6){
                int pos;
                if(turn&3){
                    while(pos=white.st[rnd.nextInt(white.size())],grid[pos]<cnt)--cnt;
                }
                else pos=white.st[rnd.nextInt(white.size())];
                int py=pos/W,px=pos%W;
                nxscore+=grid[pos];
                FOR(kr,max(0,py-pH+1),min(py+1,H-pH+1)){
                    FOR(kc,max(0,px-pW+1),min(px+1,W-pW+1)){
                        nxpena+=pvalp[kr][kc];
                    }
                }
                //if(nxscore-pf*nxpena>=curscore-pf*curpena){
                diff=nxscore-pf*nxpena*nxpena-curscore+pf*curpena*curpena;
                //diff=nxscore-pf*nxpena-curscore+pf*curpena;
                if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
                    curscore=nxscore;
                    curpena=nxpena;
                    FOR(kr,max(0,py-pH+1),min(py+1,H-pH+1)){
                        FOR(kc,max(0,px-pW+1),min(px+1,W-pW+1)){;
                            nx=++parea[kr][kc];
                            pvalm[kr][kc]=pval[nx-1]-pval[nx];
                            pvalp[kr][kc]=pval[nx+1]-pval[nx];
                        }
                    }
                    white.erase(pos);
                    black.insert(pos);
                    if(nxpena==0&&nxscore>bestscore){
                        bestscore=nxscore;
                        bestans2=black.pos;
                        //cerr<<timer.get()<<" "<<sel<<" "<<bestscore<<endl;
                    }
                }
            }
            else{
                int pos1,pos2;
                if(turn&3){
                    while(1){
                        pos1=white.st[rnd.nextInt(white.size())];
                        pos2=black.st[rnd.nextInt(black.size())];
                        if(grid[pos1]>=grid[pos2]+cnt) break;
                        else --cnt;
                    }
                }
                else{
                    pos1=white.st[rnd.nextInt(white.size())];
                    pos2=black.st[rnd.nextInt(black.size())];
                }
                int py1=pos1/W,px1=pos1%W,py2=pos2/W,px2=pos2%W;
                nxscore+=grid[pos1];
                nxscore-=grid[pos2];
                int cl,cr;
                FOR(kr,max(0,py1-pH+1),min(py1+1,H-pH+1)){
                    if(kr>py2||kr+pH-1<py2){
                        cl=max(0,px1-pW+1),cr=min(px1+1,W-pW+1);
                    }
                    else if(px1<=px2){
                        cl=max(0,px1-pW+1),cr=min(px1+1,px2-pW+1);
                    }
                    else{
                        cl=max(px2+1,px1-pW+1),cr=min(px1+1,W-pW+1);
                    }
                    FOR(kc,cl,cr){
                        nxpena+=pvalp[kr][kc];
                    }
                }
                FOR(kr,max(0,py2-pH+1),min(py2+1,H-pH+1)){
                    if(kr>py1||kr+pH-1<py1){
                        cl=max(0,px2-pW+1),cr=min(px2+1,W-pW+1);
                    }
                    else if(px2<=px1){
                        cl=max(0,px2-pW+1),cr=min(px2+1,px1-pW+1);
                    }
                    else{
                        cl=max(px1+1,px2-pW+1),cr=min(px2+1,W-pW+1);
                    }
                    FOR(kc,cl,cr){
                        nxpena+=pvalm[kr][kc];
                    }
                }
                diff=nxscore-pf*nxpena*nxpena-curscore+pf*curpena*curpena;
                //diff=nxscore-pf*nxpena-curscore+pf*curpena;
                if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
                    curscore=nxscore;
                    curpena=nxpena;
                    FOR(kr,max(0,py1-pH+1),min(py1+1,H-pH+1)){
                        if(kr>py2||kr+pH-1<py2){
                            cl=max(0,px1-pW+1),cr=min(px1+1,W-pW+1);
                        }
                        else if(px1<=px2){
                            cl=max(0,px1-pW+1),cr=min(px1+1,px2-pW+1);
                        }
                        else{
                            cl=max(px2+1,px1-pW+1),cr=min(px1+1,W-pW+1);
                        }
                        FOR(kc,cl,cr){
                            nx=++parea[kr][kc];
                            pvalm[kr][kc]=pval[nx-1]-pval[nx];
                            pvalp[kr][kc]=pval[nx+1]-pval[nx];
                        }
                    }
                    FOR(kr,max(0,py2-pH+1),min(py2+1,H-pH+1)){
                        if(kr>py1||kr+pH-1<py1){
                            cl=max(0,px2-pW+1),cr=min(px2+1,W-pW+1);
                        }
                        else if(px2<=px1){
                            cl=max(0,px2-pW+1),cr=min(px2+1,px1-pW+1);
                        }
                        else{
                            cl=max(px1+1,px2-pW+1),cr=min(px2+1,W-pW+1);
                        }
                        FOR(kc,cl,cr){
                            nx=--parea[kr][kc];
                            if(nx)pvalm[kr][kc]=pval[nx-1]-pval[nx];
                            pvalp[kr][kc]=pval[nx+1]-pval[nx];
                        }
                    }
                    white.erase(pos1);
                    white.insert(pos2);
                    black.erase(pos2);
                    black.insert(pos1);
                    //cerr<<timer.get()<<" "<<sel<<" "<<nxpena<<" "<<nxscore<<endl;
                    if(nxpena==0&&nxscore>bestscore){
                        bestscore=nxscore;
                        bestans2=black.pos;
                        //cerr<<timer.get()<<" "<<sel<<" "<<bestscore<<endl;
                    }
                }
            }
        }
    }
    /*
    inline void sfl(){
        rep(i,sflnum){
            int l=rnd.nextInt(H*W-10),r=rnd.nextInt(10);
            swap(sortnum[l],sortnum[l+r]);
        }
    }
    */
    
    vector<string> findSolution(int iH, int iW, int ih, int iw, int Kmin, int Kmax, vector<string> board) {
        timer.reset();
        H=iH,W=iW;
        pH=ih,pW=iw;
        Kmn=Kmin,Kmx=Kmax;
        black.resize(H*W);
        white.resize(H*W);
        ftr.init(W,H);

        rep(i,H)rep(j,W){
            grid[i*W+j]=board[i][j]-'0';
            vg[i%pH][j%pW]+=grid[i*W+j];
            //sortnum.pb(board[i][j]-'0',i*W+j);
        }

        int curscore=0;
        ftr.clear();


        rep(i,pH)rep(j,pW){
            sortnum.pb(vg[i][j],i*W+j);
        }
        sort(sortnum.begin(),sortnum.end(),greater<pint>());
        
        
        rep(k,Kmx){
            int py=sortnum[k].second/W,px=sortnum[k].second%W;
            rep(i,(H-1)/pH+1)if((py+i*pH)<H){
                rep(j,(W-1)/pW+1)if((px+j*pW)<W){
                    curscore+=grid[(py+i*pH)*W+(px+j*pW)];
                    ftr.add((px+j*pW)+1,(py+i*pH)+1,1);
                }
            }
        }
        bestscore=curscore;
        //double ratio=(double)Kmx/(pH*pW);
        /*
        rep(ii,H*W){
            int i=sortnum[ii].second/W,j=sortnum[ii].second%W;
            bool flag=false;
            FOR(kr,max(0,i-pH+1),min(i+1,H-pH+1)){
                FOR(kc,max(0,j-pW+1),min(j+1,W-pW+1)){
                    int tmp=ftr.sum(kc+1,kr+1,kc+pW,kr+pH);
                    if(tmp<Kmn){
                        flag=true;
                        break;
                    }
                }
                if(flag) break;
            }
            if(flag){
                curscore+=sortnum[ii].first;
                ftr.add(j+1,i+1,1);
                used[ii]=true;
            }
        }
        */
       /*
        rep(ii,H*W)if(!used[ii]){
            int i=sortnum[ii].second/W,j=sortnum[ii].second%W;
            bool flag=true;
            
            FOR(kr,max(0,i-pH+1),min(i+1,H-pH+1)){
                FOR(kc,max(0,j-pW+1),min(j+1,W-pW+1)){
                    int tmp=ftr.sum(kc+1,kr+1,kc+pW,kr+pH);
                    if(tmp>=Kmx){
                        flag=false;
                        break;
                    }
                }
                if(!flag) break;
            }
            
            if(flag){
                curscore+=sortnum[ii].first;
                ftr.add(j+1,i+1,1);
            }
        }
        */
        
       /*
        
        if(Kmx-Kmn>1||(ratio<0.3||ratio>0.7)){
            int cnt=0;
            for(int k=0;k<pH*pW;k+=2)if(cnt<Kmx){
                int py=k/pW,px=k%pW;
                if(py<pH&&px<pW){
                    curscore+=grid[py*W+px];
                    ftr.add(px+1,py+1,1);
                    ++cnt;
                    used[py][px]=true;
                }
            }
            for(int k=1;k<pH*pW;k+=2)if(cnt<Kmx){
                int py=k/pW,px=k%pW;
                if(py<pH&&px<pW){
                    curscore+=grid[py*W+px];
                    ftr.add(px+1,py+1,1);
                    ++cnt;
                    used[py][px]=true;
                }
            }
            rep(i,H)rep(j,W)if(!used[i][j]){
                if(used[i%pH][j%pW]){
                    curscore+=grid[i*W+j];
                    ftr.add(j+1,i+1,1);
                    ++cnt;
                    used[i][j]=true;
                }
            }
            bestscore=curscore;
        }
        else{
            rep(k,H){
            curscore=0;
            ftr.clear();
            rep(ii,H)rep(j,W){
                int i=(ii+k)%H;
                bool flag=true;
                FOR(kr,max(0,i-pH+1),min(i+1,H-pH+1)){
                    FOR(kc,max(0,j-pW+1),min(j+1,W-pW+1)){
                        if(ftr.sum(kc+1,kr+1,kc+pW,kr+pH)>=Kmax){
                            flag=false;
                            break;
                        }
                    }
                    if(!flag) break;
                }
                if(flag){
                    curscore+=grid[i*W+j];
                    ftr.add(j+1,i+1,1);
                }
            }
            bestscore=curscore;
            memcpy(bestans,ftr.dat,sizeof(bestans));
            }
            rep(k,W){
            ftr.clear();
            curscore=0;
            rep(jj,W)rep(i,H){
                int j=(jj+k)%W;
                bool flag=true;
                FOR(kr,max(0,i-pH+1),min(i+1,H-pH+1)){
                    FOR(kc,max(0,j-pW+1),min(j+1,W-pW+1)){
                        if(ftr.sum(kc+1,kr+1,kc+pW,kr+pH)>=Kmax){
                            flag=false;
                            break;
                        }
                    }
                    if(!flag) break;
                }
                if(flag){
                    curscore+=grid[i*W+j];
                    ftr.add(j+1,i+1,1);
                }
            }
            if(curscore>bestscore){
                bestscore=curscore;
                memcpy(bestans,ftr.dat,sizeof(bestans));
            }
            }
            memcpy(ftr.dat,bestans,sizeof(bestans));
        }
        */
        //black.clear();
        //white.clear();
        int pena=0;
        rep(i,H){
            rep(j,W){
                if(ftr.sum(j+1,i+1,j+1,i+1)>0) black.insert(i*W+j);
                else white.insert(i*W+j);
                if(i<H-pH+1&&j<W-pW+1){
                    int fsum=ftr.sum(j+1,i+1,j+pW,i+pH);
                    parea[i][j]=fsum;
                    pena+=max(0,max(Kmn-fsum,fsum-Kmx)); 
                }
            }
        }
        bestans2=black.pos;
        
        bpena=pena;
        initscore=bestscore;
        //cerr<<pena<<endl;
        /*
        int pena=0,tsum=0;
        rep(i,H-pH+1)rep(j,W-pW+1){
            tsum=ftr.sum(j+1,i+1,j+pW,i+pH);
            parea[i][j]=tsum;
            pena+=max(0,max(Kmn-tsum,tsum-Kmx)); 
        }
        */
        
            /*
            else if(bpena>0&&bpena>pena){
                bpena=pena;
                bestscore=curscore;
                memcpy(bestans,ftr.dat,sizeof(ftr.dat));
                sortnum=bsort;
            }
            sfl();
            */
            //if(bestscore>0&&loop>=100) break;
            //else sflnum=min(H*W,loop);

        /*
        memcpy(ftr.dat,bestans,sizeof(bestans));
        rep(i,H){
            rep(j,W){
                if(ftr.sum(j+1,i+1,j+1,i+1)>0) black.insert(i*W+j);
                else white.insert(i*W+j);
            }
            
        }
        */
        rep(i,pH*pW+2){
            pval[i]=max(0,max(i-Kmx,Kmn-i));
        }
        double tmpm=1.0/(2.0*65536);
        rep(i,65536){
            LN[i]=log((double)i/65536+tmpm);
            LN[i]=max(-4.0,LN[i]);
        }
        /*
        if(bpena>0){
            if(!preimprove()){
                int curscore=0;
                ftr.clear();
                rep(i,H)rep(j,W){
                    bool flag=true;
                    FOR(kr,max(0,i-pH+1),min(i+1,H-pH+1)){
                        FOR(kc,max(0,j-pW+1),min(j+1,W-pW+1)){
                            if(ftr.sum(kc+1,kr+1,kc+pW,kr+pH)>=Kmax){
                                flag=false;
                                break;
                            }
                        }
                        if(!flag) break;
                    }
                    if(flag){
                        curscore+=grid[i*W+j];
                        ftr.add(j+1,i+1,1);
                    }
                }
                black.clear();
                white.clear();
                rep(i,H){
                    rep(j,W){
                        if(ftr.sum(j+1,i+1,j+1,i+1)>0) black.insert(i*W+j);
                        else white.insert(i*W+j);
                        if(i<H-pH+1&&j<W-pW+1){
                            int fsum=ftr.sum(j+1,i+1,j+pW,i+pH);
                            parea[i][j]=fsum;
                        }
                    }
                }
                bestans2=black.pos;
                bestscore=curscore;
            }
        }
        */
        
        if(Kmx-Kmn<=2){
            TRYNM=4;
            improve_s();
            //improve_special();
            /*
           if(H*W<500||pH*pW<50){
               TRYNM=4;
               improve_s();
            }
            else{
                
                if(ratio<0.1||ratio>0.9){
                    TRYNM=4;
                    improve_s();
                }
                else{
                    //improve_special();
                    improve();
                }
            }
            */
        }
        else{
            if(((H*W<1000&&pH*pW<400)||pH*pW<200)) improve_s();
            else improve();
        }
        
        //improve();
        //improve_s();
        //improve_special();
        cerr<<bestscore<<endl;
        
        vector<string> ret;
        rep(i,H){
            string stmp;
            rep(j,W){
                if(bestans2[i*W+j]!=-1) stmp+='x';
                else stmp+='.';
            }
            ret.pb(stmp);
        }
        //rep(i,H)cerr<<ret[i]<<endl;
        return ret;
    }
};
// -------8<------- end of solution submitted to the website -------8<-------

template<class T> void getVector(vector<T>& v) {
    for (int i = 0; i < v.size(); ++i)
        cin >> v[i];
}

int main() {
    PointsOnGrid pog;
    int H;
    int W;
    int h;
    int w;
    int Kmin;
    int Kmax;
    int size;
    cin >> H;
    cin >> W;
    cin >> h;
    cin >> w;
    cin >> Kmin;
    cin >> Kmax;
    cin >> size;
    vector<string> grid(size);
    getVector(grid);

    vector<string> ret = pog.findSolution(H,W,h,w,Kmin,Kmax,grid);
    cout << ret.size() << endl;
    for (int i = 0; i < (int)ret.size(); ++i)
        cout << ret[i] << endl;
    cout.flush();
}