#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;

unsigned long xor128(){
    static unsigned long x=123456789,y=362436069,z=521288629,w=88675123;
    unsigned long t=(x^(x<<11));
    x=y;y=z;z=w;
    return (w=(w^(w >> 19))^(t^(t>>8)));
}
double timeLimit=9.9;
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
const int MAX_N=301;
struct edge{
    int src,dst;
    double cost;
    edge(int src,int dst,double cost):
    src(src),dst(dst),cost(cost){}
};
bool operator < (const edge &m,const edge &f){
    return m.cost!=f.cost?m.cost<f.cost:
    m.src!=f.src?m.src<f.src:m.dst<f.dst;
}
int data[301];
struct UnionFind{
    //int data[MAX_N]; 
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
    int size(int x){return -data[x];}
};

double kruskal(vector<edge> &ve,int n){
    sort(ve.begin(),ve.end());
    UnionFind uf;
    uf.init(n);
    double res=0;
    rep(i,ve.size()){
        edge m=ve[i];
        if(!uf.same(m.src,m.dst)){
            uf.unite(m.src,m.dst);
            res+=m.cost;
        }
    }
    return res;
}
double kruskal2(vector<edge> &ve,vector<int> &ret,int n){
    sort(ve.begin(),ve.end());
    UnionFind uf;
    uf.init(n);
    double res=0;
    rep(i,ve.size()){
        edge m=ve[i];
        if(!uf.same(m.src,m.dst)){
            uf.unite(m.src,m.dst);
            ret.pb(m.src);
            ret.pb(m.dst);
            res+=m.cost;
        }
    }
    return res;
}
class RoadsAndJunctions {
public:
    int NC,NJ;
    int sz;
    double bestscore;
    double JCOST,FPROP;
    vector<edge> base,curE,preE;
    bool used[1001][1001];
    vector<pair<double,double> > V;
    double mxcost[301],mxedge;
    vector<pair<double,pint> > cand;
    double cstfac;
    double init2(vector<edge> &ve,int n){
        sort(ve.begin(),ve.end());
        memset(mxcost,0,sizeof(mxcost));
        mxedge=0;
        //rep(i,n) g[i].clear();
        UnionFind uf;
        uf.init(n);
        double res=0;
        rep(i,ve.size()){
            edge m=ve[i];
            if(!uf.same(m.src,m.dst)){
                uf.unite(m.src,m.dst);
                mxcost[m.src]=max(mxcost[m.src],m.cost);
                mxcost[m.dst]=max(mxcost[m.dst],m.cost);
                mxedge=max(mxedge,m.cost);
                res+=m.cost;
            }
        }
        int cur=lower_bound(ve.begin(),ve.end(),edge(10001,10001,mxedge))-ve.begin();
        ve.erase(ve.begin()+cur,ve.end());
        return res;
    }
    pair<double,int> pv[101];
    void findPoint(int v){
        Point C(V[v].first,V[v].second);
        int id=0;
        rep(i,NC)if(i!=v){
            Point D(V[i].first,V[i].second);
            double dist=(D-C).abs();
            if(mxedge*1.5<=dist) continue;
            pv[id++]={dist,i};
        }
        int n=min(9,id);
        nth_element(pv,pv+n,pv+id);
        rep(i,n)FOR(j,i+1,n){
            Point L(V[pv[i].second].first,V[pv[i].second].second),R(V[pv[j].second].first,V[pv[j].second].second);
            double angle=(L-C).arg()-(R-C).arg();
            if(angle>PI) angle-=PI*2;
            else if(angle<-PI) angle+=PI*2;
            if(abs(angle)>=PI/1.67) continue;
            if(angle>0) angle=PI/3;
            else angle=-PI/3;
            Point r2=(L-C)*Point(cos(angle),sin(angle))+C,l2=(R-C)*Point(cos(-angle),sin(-angle))+C;
            Point P=CrossPoint(Line(R,r2),Line(L,l2));
            int cx=round(P.x),cy=round(P.y);
            if(cx<=0||cy<=0||cx>=sz||cy>=sz) continue;
            rep(k,NC){
                double co=hypot(V[k].first-cx,V[k].second-cy);
                if(co<=mxedge)curE.pb(k,NC,co);
            }
            double curscore=kruskal(curE,NC+1);
            if((curscore-bestscore)*(1.0-FPROP)+JCOST*cstfac<0){
                cand.pb(curscore,make_pair(cx,cy));
            }
            curE=preE;
        }
        return;
    }
    vector<int> buildJunctions(int S, vector<int> cities, double junctionCost, double failureProbability) {
        Timer timer;
        timer.reset();
        memset(used,0,sizeof(used));
        memset(mxcost,0,sizeof(mxcost));
        sz=S;NJ=0;
        cstfac=1.0;
        JCOST=junctionCost,FPROP=failureProbability;
        NC=cities.size()/2;
        V.resize(NC);
        int mnx=10000,mny=10000,mxx=0,mxy=0;
        rep(i,NC){
            V[i]={cities[i*2],cities[i*2+1]};
            mnx=min(mnx,cities[i*2]);
            mxx=max(mxx,cities[i*2]);
            mny=min(mny,cities[i*2+1]);
            mxy=max(mxy,cities[i*2+1]);
        }
        //cerr<<NC<<" "<<S<<" "<<junctionCost<<" "<<failureProbability<<endl;
        rep(i,NC)FOR(j,i+1,NC){
            double cost=hypot(V[i].first-V[j].first,V[i].second-V[j].second);
            base.pb(edge(i,j,cost));
            used[cities[i*2]][cities[i*2+1]]=true;
            used[cities[j*2]][cities[j*2+1]]=true;
        }
        bestscore=init2(base,NC);
        curE=base;preE=curE;
        rep(i,NC){
            findPoint(i);
        }
        sort(cand.begin(),cand.end());
        cand.erase(unique(cand.begin(),cand.end()),cand.end());
        int candsz=cand.size();
        candsz=min(NC*2,candsz);
        vector<int> ret;
        rep(i,candsz){
            int cx=cand[i].second.first,cy=cand[i].second.second;
            rep(j,NC+NJ){
                double co=hypot(V[j].first-cx,V[j].second-cy);
                if(co<=mxedge)curE.pb(j,NC+NJ,co);
            }
            double curscore=kruskal(curE,NC+NJ+1);
            int cnt1=(bestscore-curscore)*(1.0-FPROP)/(JCOST*cstfac+0.001);
            if(cnt1<1){
                curE=preE;
                continue;
            }
            bestscore=curscore;
            int cnt2=1;
            if(xor128()%100<=FPROP*100&&cnt1>4) ++cnt2;
            if(xor128()%100<=FPROP*100&&cnt1>10) ++cnt2;
            cnt1=0;
            rep(l,3){
                if(NJ==NC*2) break;
                if(cx+l<sz&&!used[cx+l][cy]){
                    ret.pb(cx+l),ret.pb(cy);
                    used[cx+l][cy]=true;
                    V.pb(cx+l,cy);
                    rep(i,NC+NJ){
                        double co=hypot(V[i].first-cx-l,V[i].second-cy);
                        if(co<=mxedge)curE.pb(i,NC+NJ,co);
                    }
                    ++cnt1;++NJ;
                    if(cnt1==cnt2){
                        break;
                    } 
                }
                if(NJ==NC*2) break;
                if(cx-l>=0&&!used[cx-l][cy]){
                    ret.pb(cx-l),ret.pb(cy);
                    V.pb(cx-l,cy);
                    rep(i,NC+NJ){
                        double co=hypot(V[i].first-cx+l,V[i].second-cy);
                        if(co<=mxedge)curE.pb(i,NC+NJ,co);
                    }
                    used[cx-l][cy]=true;
                    ++cnt1;++NJ;
                    if(cnt1==cnt2){
                        break;
                    }
                }    
            }
            preE=curE;
            if(NJ==NC*2) break;
        }
        /*dfs(0,-1,ret);
        bestscore=init(curE,NC+NJ);
        rect(ret);*/
        bestscore=init2(curE,NC+NJ);
        preE=curE;
        bool flag=true;
        double sttime=timer.get(),sub=0;
        while(1){
            if(NJ==NC*2) break;
            int cx,cy,id;
            if(timer.get()>timeLimit-sub) break;
            int cnt=0;
            while(1){
                if(cnt==100) break;
                id=xor128()%(NC+NJ);
                int range=(mxcost[id]+1)*1.2;
                int dx=-range/2+xor128()%range,dy=-range/2+xor128()%range;
                cx=V[id].first+dx,cy=V[id].second+dy;
                if(cx>mnx&&cy>mny&&cx<mxx&&cy<mxy&&!used[cx][cy]) break;
                ++cnt;
            }
            if(cnt>=100) break;
            double mx=0,co;
            rep(i,NC+NJ){
                co=hypot(V[i].first-cx,V[i].second-cy);
                if(co<=mxedge){
                    curE.pb(i,NC+NJ,co);
                    mx=max(co,mx);
                }
            }
            mxcost[id]=mx;
            ++NJ;
            double curscore=kruskal(curE,NC+NJ);
            //curscore=(curscore-bestscore)*(1.0-failureProbability)+bestscore+junctionCost;
            if((curscore-bestscore)*(1.0-failureProbability*1.1)+junctionCost*1.15<0){
                bestscore=curscore;
                V.pb(cx,cy);
                ret.pb(cx),ret.pb(cy);
                used[cx][cy]=true;
                preE=curE;
            }
            else{
                --NJ;
                curE=preE;
            }
            if(flag){
                flag=false;
                sub=timer.get()-sttime;
                sub*=2;
            }
        }
        return ret;
    }
    vector<int> buildRoads(vector <int> junctionStatus) {
        vector<int> ans;
        rep(i,NC+NJ)FOR(j,NC,NC+NJ){
            if(i>=NC&&j>=NC&&i>=j) continue;
            if(i>=NC&&junctionStatus[i-NC]==0||j>=NC&&junctionStatus[j-NC]==0) continue;
            double cost=hypot(V[i].first-V[j].first,V[i].second-V[j].second);
            base.pb(edge(i,j,cost));
        }
        double bestscore=kruskal2(base,ans,NC+NJ);
        
        return ans;
    }
};
// -------8<------- end of solution submitted to the website -------8<-------

template<class T> void getVector(vector<T>& v) {
    for (int i = 0; i < v.size(); ++i)
        cin >> v[i];
}

int main() {
    RoadsAndJunctions rj;
    int S, C;
    cin >> S >> C;
    vector<int> cities(C);
    getVector(cities);
    double junctionCost, failureProbability;
    cin >> junctionCost >> failureProbability;

    vector<int> ret = rj.buildJunctions(S, cities, junctionCost, failureProbability);
    cout << ret.size() << endl;
    for (int i = 0; i < (int)ret.size(); ++i)
        cout << ret[i] << endl;
    cout.flush();
    
    int J;
    cin >> J;
    vector<int> junctionStatus(J);
    getVector(junctionStatus);

    ret = rj.buildRoads(junctionStatus);
    cout << ret.size() << endl;
    for (int i = 0; i < (int)ret.size(); ++i)
        cout << ret[i] << endl;
    cout.flush();
}