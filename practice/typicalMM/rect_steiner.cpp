//simple SA
#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

const int dx[]={-1,0,1,0},dy[]={0,-1,0,1};
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

double PI=acos(-1.0);
#define eps (1e-10)
struct Point{
    double x,y;
    Point(){}
    Point(double x,double y):x(x),y(y){}
    Point operator+(Point p) {return Point(x+p.x,y+p.y);}
    Point operator-(Point p) {return Point(x-p.x,y-p.y);}
    Point operator*(Point p) {return Point(x*p.x-y*p.y,x*p.y+y*p.x);}
    Point operator*(double k){return Point(x*k,y*k);}
    Point operator/(double k){return Point(x/k,y/k);}
    double norm(){return x*x+y*y;}
    double abs(){return sqrt(norm());}
    bool operator == (const Point &p) const{return fabs(x-p.x)<eps && fabs(y-p.y)<eps;}
    bool operator != (const Point &p) const{return !(fabs(x-p.x)<eps && fabs(y-p.y)<eps);}
    double arg(){return atan2(y,x);}
    double arg2(){double d=atan2(y,x);return d>=0?d:d+2.0*PI;}
    double dot(Point p){return x*p.x+y*p.y;}
    double det(Point p){return x*p.y-y*p.x;}
};
struct Line{
    Point p1,p2;
    Line(){}
    Line(Point p1, Point p2):p1(p1),p2(p2){}
};
Point CrossPoint(Line l1,Line l2){
    double a=(l1.p2-l1.p1).det(l2.p2-l2.p1);
    double b=(l1.p2-l1.p1).det(l1.p2-l2.p1);
    if(fabs(a)<eps&&fabs(b)<eps) return l2.p1;
    return l2.p1+(l2.p2-l2.p1)*(b/a);
}
struct vset{
    vector<int> st,pos;
    vset(int n){
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
};
struct edge{
    int src,dst,cost;
    edge(){};
    edge(int src,int dst,int cost):
    src(src),dst(dst),cost(cost){}
};
bool operator < (const edge &m,const edge &f){
    return m.cost!=f.cost?m.cost<f.cost:
    m.src!=f.src?m.src<f.src:m.dst<f.dst;
}


int data[10002]; 
struct UnionFind{
    void init(int n){memset(data,-1,sizeof(data));}
    int find(int x){return data[x]<0?x:data[x]=find(data[x]);}
    void unite(int x, int y){
        x=find(x);y=find(y);       
        if(x!=y){
            if(data[y]<data[x]) swap(x,y);
            data[x]+=data[y];data[y]=x;
        }
        return;
    } 
    bool same(int x, int y){return (find(x) == find(y));}
    int size(int x){return -data[find(x)];}
};

const int W=100;
Timer timer;
int N,M;
int base[10001];
vset spo(10001);
vector<int> bestpo;
vector<edge> inite,be,bbe,fbe,tbe,bestbe;
int mxedge=0;
int bestscore=0;
int initkrus(){
    sort(inite.begin(),inite.end());
    UnionFind uf;
    uf.init(10001);
    double res=0;
    rep(i,inite.size()){
        edge m=inite[i];
        if(!uf.same(m.src,m.dst)){
            uf.unite(m.src,m.dst);
            be.emplace_back(edge(m.src,m.dst,m.cost));
            res+=m.cost-1;
            mxedge=max(mxedge,m.cost);
        }
    }
    return res;
}
int kruskal(bool flag=false){
    tbe=be;
    sort(tbe.begin(),tbe.end());
    UnionFind uf;
    uf.init(10001);
    int res=0;
    rep(i,tbe.size()){
        edge m=tbe[i];
        if(!uf.same(m.src,m.dst)){
            uf.unite(m.src,m.dst);
            if(flag)fbe.emplace_back(edge(m.src,m.dst,m.cost));
            res+=m.cost-1;
        }
    }
    if(M==0) return res+1;
    else return res+M;
}
pint pv[101];
vector<pint> cand;
void findpoint(int v){
    int vx=spo.st[v]%W,vy=spo.st[v]/W;
    int id=0;
    rep(i,N)if(i!=v){
        int dist=abs(vx-spo.st[i]%W)+abs(vy-spo.st[i]/W);
        if(mxedge*2<=dist) continue;
        pv[id++]={dist,i};
    }
    int n=min(9,id);
    nth_element(pv,pv+n,pv+id);
    Point C(vx,vy);
    rep(i,n)FOR(j,i+1,n){
        int pz=be.size();
        Point L(spo.st[pv[i].second]%W,spo.st[pv[i].second]/W),R(spo.st[pv[j].second]%W,spo.st[pv[j].second]/W);
        double angle=(L-C).arg()-(R-C).arg();
        if(angle>PI) angle-=PI*2;
        else if(angle<-PI) angle+=PI*2;
        if(abs(angle)>=PI/1.67) continue;
        if(angle>0) angle=PI/3;
        else angle=-PI/3;
        Point r2=(L-C)*Point(cos(angle),sin(angle))+C,l2=(R-C)*Point(cos(-angle),sin(-angle))+C;
        Point P=CrossPoint(Line(R,r2),Line(L,l2));
        int cx=round(P.x),cy=round(P.y);
        if(cx<0||cy<0||cx>=W||cy>=W) continue;
        if(base[cy*W+cx]) continue;
        rep(k,N){
            int co=abs(spo.st[k]%W-cx)+abs(spo.st[k]/W-cy);
            if(co<=mxedge)be.pb(edge(spo.st[k],cy*W+cx,co));
        }
        int curscore=kruskal();
        if(curscore-bestscore<0){
            cand.pb(curscore,cy*W+cx);
        }
        be.resize(pz);
    }
}
inline bool forceupdate(double diff,double temp){
    if(diff>=0) return true;
    double d=(diff)*temp;
    if(d<-6) return false;
    return exp(d)>rnd.nextDouble();
}
void improve(){
    double start=timer.get();
    int turn=0;
    int diff;
    double t=start,invtl=1.0/timeLimit;
    int curscore=bestscore,prescore;
    double starttemp=1,endtemp=0.1,curtemp=1.0/starttemp;
    while(1){
        ++turn;
        if((turn&15)==0){
            t=timer.get();
            curtemp=1.0/(starttemp+(endtemp-starttemp)*(t-start)*invtl);
        }
        if(t>timeLimit){
            be=bestbe;
            cerr<<turn<<endl;
            return;
        }
        int sel=rnd.nextInt(3);//moveなし
        
        prescore=curscore;
        if(sel==0&&M!=0){
            //delete
            be=bbe;
            int id=rnd.nextInt(M)+N;
            int pos=spo.st[id];
            spo.erase(pos);
            --M;
            FOR(i,0,N)FOR(j,N,N+M){
                int co=abs(spo.st[i]%W-spo.st[j]%W)+abs(spo.st[i]/W-spo.st[j]/W);
                if(co<=mxedge)be.pb(edge(spo.st[i],spo.st[j],co));
            }
            FOR(i,N,N+M)FOR(j,i+1,N+M){
                int co=abs(spo.st[i]%W-spo.st[j]%W)+abs(spo.st[i]/W-spo.st[j]/W);
                if(co<=mxedge)be.pb(edge(spo.st[i],spo.st[j],co));
            }
            curscore=kruskal();
            
            diff=prescore-curscore;
            if(forceupdate(diff,curtemp)){
                
                if(bestscore>curscore){
                    bestscore=curscore;
                    bestbe=be;
                    bestpo=spo.st;
                }
            }
            else{
                curscore=prescore;
                spo.insert(pos);
                ++M;
            }
        }
        else if(sel==1||M==0){
            //add
            be=bbe;
            int pos;
            while(1){
                pos=rnd.nextInt(10000);
                if(spo.pos[pos]==-1) break;
            }
            spo.insert(pos);
            ++M;
            FOR(i,0,N)FOR(j,N,N+M){
                int co=abs(spo.st[i]%W-spo.st[j]%W)+abs(spo.st[i]/W-spo.st[j]/W);
                if(co<=mxedge)be.pb(edge(spo.st[i],spo.st[j],co));
            }
            FOR(i,N,N+M)FOR(j,i+1,N+M){
                int co=abs(spo.st[i]%W-spo.st[j]%W)+abs(spo.st[i]/W-spo.st[j]/W);
                if(co<=mxedge)be.pb(edge(spo.st[i],spo.st[j],co));
            }
            curscore=kruskal();
            diff=prescore-curscore;
            if(forceupdate(diff,curtemp)){
                
                if(bestscore>curscore){
                    bestscore=curscore;
                    bestbe=be;
                    bestpo=spo.st;
                }
            }
            else{
                curscore=prescore;
                spo.erase(pos);
                --M;
            }
        }
        else{
            //move
            be=bbe;
            int id=rnd.nextInt(M)+N;
            int pos=spo.st[id],npos=-1;
            rep(i,4){
                int k=rnd.nextInt()&3;
                if(pos/W+dy[k]>=0&&pos/W+dy[k]<W&&pos%W+dx[k]>=0&&pos%W+dx[k]<W
                &&spo.pos[(pos/W+dy[k])*W+pos%W+dx[k]]==-1){
                    npos=(pos/W+dy[k])*W+pos%W+dx[k];
                    break;
                }
            }
            if(npos==-1) continue;
            spo.erase(pos);
            spo.insert(npos);
            FOR(i,0,N)FOR(j,N,N+M){
                int co=abs(spo.st[i]%W-spo.st[j]%W)+abs(spo.st[i]/W-spo.st[j]/W);
                if(co<=mxedge)be.pb(edge(spo.st[i],spo.st[j],co));
            }
            FOR(i,N,N+M)FOR(j,i+1,N+M){
                int co=abs(spo.st[i]%W-spo.st[j]%W)+abs(spo.st[i]/W-spo.st[j]/W);
                if(co<=mxedge)be.pb(edge(spo.st[i],spo.st[j],co));
            }
            curscore=kruskal();
            diff=prescore-curscore;
            if(forceupdate(diff,curtemp)){
                
                if(bestscore>curscore){
                    bestscore=curscore;
                    bestbe=be;
                    bestpo=spo.st;
                }
            }
            else{
                curscore=prescore;
                spo.insert(pos);
                spo.erase(npos);
            }
        }
    }
}
int main(){
    timer.reset();
    cin>>N;
    int xi,yi;
    rep(i,N){
        cin>>xi>>yi;
        base[yi*W+xi]=true;
        spo.insert(yi*W+xi);
    }
    rep(i,N)FOR(j,i+1,N){
        int tcost=abs(spo.st[i]/W-spo.st[j]/W)+abs(spo.st[i]%W-spo.st[j]%W);
        inite.emplace_back(edge(spo.st[i],spo.st[j],tcost));
    }
    bestscore=initkrus();
    bbe=be;

    rep(i,N){
        findpoint(i);
    }

    sort(cand.begin(),cand.end());
    cand.erase(unique(cand.begin(),cand.end()),cand.end());
    int candsz=cand.size();
    M=0;
    rep(i,candsz){
        int pz=be.size();
        int cx=cand[i].second%W,cy=cand[i].second/W;
        rep(j,N+M){
            int co=abs(spo.st[j]%W-cx)+abs(spo.st[j]/W-cy);
            if(co<=mxedge)be.pb(edge(spo.st[j],cy*W+cx,co));
        }
        ++M;
        int curscore=kruskal();
        if(curscore<bestscore){
            bestscore=curscore;
            spo.insert(cy*W+cx);
            
        }
        else{
            --M;
            be.resize(pz);
        }
    }

    bestbe=be;
    improve();
    be=bestbe;

    kruskal(true);
    
    cerr<<bestscore<<endl;

    set<pint> addPanel;
    rep(i,fbe.size()){
        edge m=fbe[i];
        int fx=m.src%W,tx=m.dst%W,fy=m.src/W,ty=m.dst/W;
        int mv=(fx<=tx)?1:-1;
        while(fx!=tx){
            fx+=mv;
            addPanel.insert({fx,fy});
        }
        mv=(fy<=ty)?1:-1;
        while(fy!=ty){
            fy+=mv;
            addPanel.insert({fx,fy});
        }
    }
    rep(i,bestpo.size()){
        addPanel.insert({bestpo[i]%W,bestpo[i]/W});
    }
    vector<pint> ans;
    for(auto it:addPanel){
        if(!base[it.second*W+it.first]) ans.emplace_back(it);
    }
    cout<<ans.size()<<endl;
    for(auto it:ans){
        cout<<it.first<<" "<<it.second<<endl;
    }
    return 0;
}