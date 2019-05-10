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
    bool operator < (const Point &f)const {
        return x!=f.x?x<f.x:y<f.y;
    }
};

struct Circle{
    double r;
    Point p;
    Circle(){}
    Circle(Point p,double r):p(p),r(r){}
};
bool isIntersectCC(Circle c1,Circle c2){
    return (c1.p-c2.p).abs()<=c1.r+c2.r+eps;
}
pair<Point,Point> CrossPointsCC(Circle c1,Circle c2){
    assert(isIntersectCC(c1,c2));
    double d=(c1.p-c2.p).abs();
    double k=acos((d*d+c1.r*c1.r-c2.r*c2.r)/(c1.r*d*2));
    return make_pair(c1.p+(c2.p-c1.p)*Point(cos(k),sin(k))*(c1.r/d),c1.p+(c2.p-c1.p)*Point(cos(-k),sin(-k))*(c1.r/d));
}
Timer timer;
int N,R;
int X[1001],Y[1001];
bool used[1001];
Point P[1001];
vector<pint> cand;
vector<Point> ans,best;
int main(){
    timer.reset();
    cin>>N>>R;
    rep(i,N){
        cin>>X[i]>>Y[i];
        P[i]=Point(X[i],Y[i]);
    }
    rep(i,N)FOR(j,i+1,N){
        if((P[i]-P[j]).abs()<R) cand.emplace_back(i,j);
    }
    int sz=cand.size();
    int bestscore=100001;
    vector<pair<int,Point> > pm;
    rep(lp,100){
        if(timer.get()>timeLimit) break;
    int cvcnt=0;
    ans.clear();
    
    memset(used,0,sizeof(used));
    while(1){
        //int mx=0;
        Point nxP,p1,p2;
        pm.clear();
        rep(i,sz){
            
            int l=cand[i].first,r=cand[i].second;
            pair<Point,Point> ret=CrossPointsCC(Circle(P[l],R),Circle(P[r],R));
            p1=ret.first,p2=ret.second;
            p1.x=floor(p1.x);p1.y=floor(p1.y);p2.x=floor(p2.x);p2.y=floor(p2.y);
            rep(k,4){
                Point tp=p1;
                tp.x+=(k&1);tp.y+=(k>>1&1);
                if((tp-P[r]).abs()<=R&&(tp-P[l]).abs()<=R&&tp.x>=0&&tp.x<1000&&tp.y>=0&&tp.y<1000){
                    p1=tp;
                    break;
                }
            }
            rep(k,4){
                Point tp=p2;
                tp.x+=(k&1);tp.y+=(k>>1&1);
                if((tp-P[r]).abs()<=R&&(tp-P[l]).abs()<=R&&tp.x>=0&&tp.x<1000&&tp.y>=0&&tp.y<1000){
                    p2=tp;
                    break;
                }
            }
            /*
            if(p1.x<p2.x){
                p1.x=ceil(p1.x),p2.x=floor(p2.x);
            }
            else{
                p1.x=floor(p1.x),p2.x=ceil(p2.x);
            }
            if(p1.y<p2.y){
                p1.y=ceil(p1.y),p2.y=floor(p2.y);
            }
            else{
                p1.y=floor(p1.y),p2.y=ceil(p2.y);
            }
            */
            int cnt=0;
            if(p1.x>=0&&p1.x<=1000&&p1.y>=0&&p1.y<=1000){
                rep(j,N)if(!used[j]){
                    if((p1-P[j]).abs()<=R) ++cnt;
                }
            }
            if(cnt!=0)pm.emplace_back(cnt,p1);
            /*
            if(cnt>mx){
                mx=cnt;
                nxP=p1;
            }
            */
            cnt=0;
            if(p2.x>=0&&p2.x<=1000&&p2.y>=0&&p2.y<=1000){
                rep(j,N)if(!used[j]){
                    if((p2-P[j]).abs()<=R) ++cnt;
                }
            }
            if(cnt!=0)pm.emplace_back(cnt,p2);
            /*
            if(cnt>mx){
                mx=cnt;
                nxP=p2;
            }
            */
            
        }
        sort(pm.begin(),pm.end());
        reverse(pm.begin(),pm.end());
        if(lp==0&&pm.size()>0) nxP=pm[0].second;
        else if(pm.size()>0){
            nxP=pm[rnd.nextInt(min((int)pm.size(),3))].second;
        }
        ans.emplace_back(nxP);
        int cnt=0;
        rep(i,N)if(!used[i]){
            if((nxP-P[i]).abs()<=R){
                used[i]=true;
                ++cnt;
            }
        }
        cvcnt+=cnt;
        if(cnt==0||pm.size()==0) break;
    }
    rep(i,N)if(!used[i]){
        ans.emplace_back(P[i]);
    }
    if(bestscore>ans.size()){
        bestscore=ans.size();
        best=ans;
    }
    }
    cout<<best.size()<<endl;
    rep(i,best.size()){
        cout<<(int)best[i].x<<" "<<(int)best[i].y<<endl;
    }
}