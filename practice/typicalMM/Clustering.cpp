#include <bits/stdc++.h>
using namespace std;
#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define rep(i,n)   FOR(i,0,n)
#define pb emplace_back
typedef long long ll;
typedef pair<int,int> pint;


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

int N,K;
int posX[1001],posY[1001];
int cX[21],cY[21],cl[1001],ncl[1001],cnt[21];
int pX[21],pY[21];
int bcX[21],bcY[21];
double bestscore;
double kmeans(bool init=true){
    if(init)rep(i,N) cl[i]=rnd.nextInt(K);

    rep(i,100){
        memset(cX,0,sizeof(cX));
        memset(cY,0,sizeof(cY));
        memset(cnt,0,sizeof(cnt));
        rep(i,N){
            int id=cl[i];
            cX[id]+=posX[i];
            cY[id]+=posY[i];
            ++cnt[id];
        }
        rep(i,K){
            if(cnt[i]!=0){
                cX[i]/=cnt[i];
                cY[i]/=cnt[i];
            }
            else{
                //check collision
                cX[i]=rnd.nextInt(1000);
                cY[i]=rnd.nextInt(1000);
            }
        }
        bool flag=true;
        rep(i,K){
            if(cX[i]!=pX[i]||cY[i]!=pY[i]) flag=false;
            pX[i]=cX[i];pY[i]=cY[i];
        }
        if(flag) break;
        rep(i,N){
            double mn=1e9,dist;
            int nid=-1;
            rep(j,K){
                dist=hypot(cX[j]-posX[i],cY[j]-posY[i]);
                if(mn>dist){
                    mn=dist;nid=j;
                }
            }
            ncl[i]=nid;
        }
        memcpy(cl,ncl,sizeof(ncl));
    }

    double ret=0;
    int id;
    rep(i,N){
        id=cl[i];
        ret+=hypot(cX[id]-posX[i],cY[id]-posY[i]);
    }
    return ret;
}
double dist2[1001];
bool used[1001];
double kmeanspp(){
    vector<int> center;
    memset(used,0,sizeof(used));
    center.emplace_back(rnd.nextInt(N));
    used[center[0]]=true;
    FOR(lp,1,K){
        double tot=0,mn=1e9;
        rep(i,N){
            rep(j,center.size()){
                double td=(posX[i]-posX[center[j]])*(posX[i]-posX[center[j]])+(posY[i]-posY[center[j]])*(posY[i]-posY[center[j]]);
                if(mn>td) mn=td;
            }
            dist2[i]=mn;
        }
        FOR(i,1,N) dist2[i]+=dist2[i-1];
        tot=dist2[N-1];
        double sel=rnd.nextDouble()*tot;
        int nx=N-1;
        rep(i,N){
            if(dist2[i]>=sel&&!used[i]){
                nx=i;
                break;
            }
        }
        center.emplace_back(nx);
        used[nx]=true;
    }
    rep(i,N){
        double mn=1e9,dist;
        int id=-1;
        rep(jj,center.size()){
            int j=center[jj];
            dist=hypot(posX[j]-posX[i],posY[j]-posY[i]);
            if(mn>dist){
                mn=dist;
                id=jj;
            }
        }
        cl[i]=id;
    }
    return kmeans(false);
}
int main(){
    cin>>N>>K;
    rep(i,N) cin>>posX[i]>>posY[i];
    bestscore=1e9;

    double score;
    rep(i,10){
        score=kmeans();
        cerr<<"k-means:"<<score<<endl;
        if(bestscore>score){
            bestscore=score;
            memcpy(bcX,cX,sizeof(cX));
            memcpy(bcY,cY,sizeof(cY));
        }
    }
    cerr<<bestscore<<endl;
    rep(i,10){
        score=kmeanspp();
        cerr<<"k-means++:"<<score<<endl;
        if(bestscore>score){
            bestscore=score;
            memcpy(bcX,cX,sizeof(cX));
            memcpy(bcY,cY,sizeof(cY));
        }
    }
    cerr<<bestscore<<endl;
    rep(i,K){
        cout<<bcX[i]<<" "<<bcY[i]<<endl;
    }
    return 0;
}
