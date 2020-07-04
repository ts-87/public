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

struct UnionFind{
    int n;
    vector<int> data;
    UnionFind(int _n):n(_n),data(_n,-1){}
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
double LN[65536];
const int dx[]={0,0,-1,1},dy[]={-1,1,0,0},dk[]={0,2,1,4};
string dir="UDLR";
int N,P,M;
string g1[40],g2[40];
vector<int> cpos[26];
int cy,cx;
vector<pint> pile,bpile;

vector<UnionFind> ufv;
vector<pint> connsz;
vector<pint> g[1600];
vector<int> vlist;
int dist[1601][1601];
bool used[1601];
const int INF=0x3f3f3f3f;
vector<int> perm,bperm,nei[1600];
int pos_id[1601];
void t_opt(int st){
    //initgreedy
    int curcost=0;
    perm.pb(st);pos_id[st]=0;
    int sz=vlist.size();
    memset(used,0,sizeof(used));
    used[st]=true;
    FOR(i,1,sz){
        int mn=100000,id=-1;
        rep(j,sz)if(!used[vlist[j]]){
            if(mn>dist[perm.back()][vlist[j]])mn=dist[perm.back()][vlist[j]],id=vlist[j];
        }
        if(id!=-1){
            used[id]=true,pos_id[id]=perm.size();perm.pb(id),curcost+=mn;
        }
        else break;
    }
    int bestcost=curcost;
    perm.pb(N*N);
    //2opt
    int loopnum=1500000/sz,diff;
    double curtemp,starttemp=15,endtemp=0.001,ts;
    double invtl=1.0/loopnum;
    cerr<<"tsp:"<<curcost<<endl;
    rep(loop,loopnum){
        int id1,id11,id2,id22;
        ts=starttemp+(endtemp-starttemp)*loop*invtl;
        FOR(i,0,sz)if(vlist[i]!=st){
            int rd=vlist[i];
            rep(j,nei[rd].size()){
                id2=nei[rd][j];
                if(id2==st) continue;
                id22=pos_id[id2];
                id1=rd;
                id11=pos_id[id1];
                if(id22<id11) swap(id11,id22),swap(id2,id1);

                diff=dist[id1][perm[id11-1]]+dist[id2][perm[id22+1]]
                -dist[id2][perm[id11-1]]-dist[id1][perm[id22+1]];
		        if(diff>=0||diff>LN[rnd.nextInt()&65535]*ts){
                    curcost-=diff;
                    reverse(perm.begin()+id11,perm.begin()+id22+1);
                    FOR(k,id11,id22+1) pos_id[perm[k]]=k;
                    if(bestcost>curcost){
                        bestcost=curcost;
                        bperm=perm;
                    }
                }
            }
        }
    }
    perm.pop_back();
    cerr<<sz<<" "<<bestcost<<endl;
}
int ope_move(int turn,int st,int rch){
    int ct=turn;
    int px=st%N,py=st/N;
    FOR(i,1,perm.size()){
        int nx=perm[i]%N,ny=perm[i]/N;
        while(1){
            int mn=10000,ttx,tty,ttd=-1,tpp,cdist=dist[py*N+px][ny*N+nx];
            rep(k,4){
                int tx=px,ty=py;
                pile.clear();
                bool ok=false;
                while(1){
                    tx+=dx[k],ty+=dy[k];
                    if(ty>=0&&tx>=0&&ty<N&&tx<N){
                        if(g1[ty][tx]=='x') pile.pb(ty,tx);
                        if((g2[ty][tx]==' '||(g2[ty][tx]-'A')==rch)&&dist[ty*N+tx][ny*N+nx]<cdist){
                            int pp=0;
                            if(ty+dy[k]>=0&&tx+dx[k]>=0&&ty+dy[k]<N&&tx+dx[k]<N&&g1[ty+dy[k]][tx+dx[k]]=='-') pp=1;
                
                            int tmp=dist[ty*N+tx][ny*N+nx];
                            if(mn>tmp||tmp==0&&(g2[ty][tx]-'A')==rch){
                                mn=tmp;ttx=tx,tty=ty,ttd=k,tpp=pp;bpile=pile;
                            }
                        }
                    }
                    else{
                        break;
                    }
                }
                /*
                if(!ok) continue;
                int pp=0;
                if(ty+dy[k]>=0&&tx+dx[k]>=0&&ty+dy[k]<N&&tx+dx[k]<N&&g1[ty+dy[k]][tx+dx[k]]=='-') pp=1;
                
                int tmp=(int)pile.size()+pp;
                if(mn>tmp){
                    mn=tmp;ttx=tx,tty=ty,ttd=k,tpp=pp;bpile=pile;
                }
                */
            }

            int px2,py2;
            if(tpp!=0&&bpile.size()==0){
                while(1){
                    py2=rnd.nextInt(N),px2=rnd.nextInt(N);
                    if(g1[py2][px2]=='x'){
                        break;
                    }
                }
                cout<<"P "<<py2<<" "<<px2<<" "<<tty+dy[ttd]<<" "<<ttx+dx[ttd]<<endl;
                g1[py2][px2]='-',g1[tty+dy[ttd]][ttx+dx[ttd]]='x';
                ++ct;
                if(ct>=M) return ct;
            }
            rep(i,bpile.size()){
                if(i==0&&tpp!=0){
                    cout<<"P "<<bpile[i].first<<" "<<bpile[i].second<<" "<<tty+dy[ttd]<<" "<<ttx+dx[ttd]<<endl;
                    g1[bpile[i].first][bpile[i].second]='-',g1[tty+dy[ttd]][ttx+dx[ttd]]='x';
                    ++ct;
                    if(ct>=M) return ct;
                    
                }
                else{
                    while(1){
                        py2=rnd.nextInt(N),px2=rnd.nextInt(N);
                        if(py!=py2&&px!=px2&&g1[py2][px2]=='-'){
                            break;
                        }
                    }
                    cout<<"P "<<bpile[i].first<<" "<<bpile[i].second<<" "<<py2<<" "<<px2<<endl;
                    g1[bpile[i].first][bpile[i].second]='-',g1[py2][px2]='x';
                    ++ct;
                    if(ct>=M) return ct;
                }
                
            }

            cout<<dir[ttd]<<endl;g2[tty][ttx]=' ';
            ++ct;
            if(ct>=M) return ct;
            py=tty,px=ttx;
            if(ny==py&&nx==px) break;
        }
        if(ct>=M) return ct;
    }
    cy=py,cx=px;
    return ct;
}
int main(){
    ios::sync_with_stdio(false),cin.tie(0),cout.tie(0);
    cin>>N>>P>>M;
    rep(i,N) cin>>g1[i];
    rep(i,N) cin>>g2[i];
    rep(i,N)rep(j,N){
        if(g1[i][j]=='o'){
            cy=i,cx=j;
            g1[i][j]='-';
        }
        cpos[g2[i][j]-'A'].pb(i*N+j);
    }
    double tmpm=1.0/(2.0*65536);
    rep(i,65536){
        LN[i]=log((double)i/65536+tmpm);
    }

    rep(i,26){
        UnionFind uf(N*N);
        rep(k1,cpos[i].size())FOR(k2,k1+1,cpos[i].size()){
            if(cpos[i][k1]/N==cpos[i][k2]/N||cpos[i][k1]%N==cpos[i][k2]%N) uf.unite(cpos[i][k1],cpos[i][k2]);
        }
        ufv.pb(uf);
        int mx=0,tv=-1;
        rep(k,cpos[i].size()){
            int tmp=uf.size(cpos[i][k]);
            if(mx<tmp) mx=tmp,tv=(cpos[i][k]*26+i);
        }
        connsz.pb(mx,tv);
    }
    sort(connsz.begin(),connsz.end(),greater<pint>());
    memset(dist,0x3f,sizeof(dist));
    rep(i,N*N) dist[i][N*N]=0,dist[N*N][i]=0;
    int rch=connsz[0].second%26,rpoint=connsz[0].second/26;
    int turn=0;
    queue<int> que;
    while(turn<M){
        vlist.clear();
        rep(i,cpos[rch].size())if(ufv[rch].same(rpoint,cpos[rch][i])) vlist.pb(cpos[rch][i]);
        /*
        rep(i,vlist.size()){
            g[vlist[i]].clear();
            rep(j,vlist.size())if(i!=j){
                if(vlist[i]/N==vlist[j]/N||vlist[i]%N==vlist[j]%N)g[vlist[i]].pb(1,vlist[j]);
            }
        }
        */
       rep(i,N)rep(j,N)if(g2[i][j]==' '){
            vlist.pb(i*N+j);
            g[i*N+j].clear();
        }
        rep(i,vlist.size()){
            g[vlist[i]].clear();
            rep(j,vlist.size())if(i!=j){
                if(vlist[i]/N==vlist[j]/N||vlist[i]%N==vlist[j]%N){
                    int tmp=abs(vlist[i]/N-vlist[j]/N)+abs(vlist[i]%N-vlist[j]%N);
                    g[vlist[i]].pb(10+tmp/3,vlist[j]);
                }
                else if(g2[vlist[i]/N][vlist[j]%N]==' '||g2[vlist[j]/N][vlist[i]%N]==' '){
                    int tmp=abs(vlist[i]/N-vlist[j]/N)+abs(vlist[i]%N-vlist[j]%N);
                    g[vlist[i]].pb(15+tmp/3,vlist[j]);
                }
            }
        }
        
        
        rep(i,vlist.size()){
            int ty=vlist[i]/N,tx=vlist[i]%N;
            rep(j,N)if(g2[ty][j]==' '){
                g[vlist[i]].pb(10+j/3,ty*N+j);
                g[ty*N+j].pb(10+j/3,vlist[i]);
            }
            rep(j,N)if(g2[j][tx]==' '){
                g[vlist[i]].pb(10+j/3,j*N+tx);
                g[j*N+tx].pb(10+j/3,vlist[i]);
            }
        }
        
        int sz=vlist.size();
        priority_queue<pint,vector<pint>,greater<pint> > pq;
        rep(i,sz){
            int st=vlist[i];
            pq.push({0,st});
            dist[st][st]=0;
            int cv,td;
            while(!pq.empty()){
                int cv,td;
                tie(td,cv)=pq.top();pq.pop();
                if(dist[st][cv]<td) continue;
                for(auto it:g[cv])if(dist[st][cv]+it.first<dist[st][it.second]){
                    dist[st][it.second]=dist[st][cv]+it.first;
                    pq.push({dist[st][it.second],it.second});
                }
            }
        }
        /*
        //vlist.pb(cy*N+cx);
        int sz=vlist.size();
        rep(i,sz){
            int st=vlist[i];
            memset(used,0,sizeof(used));
            que.push(st);used[st]=true;
            dist[st][st]=0;
            while(!que.empty()){
                int cv=que.front();que.pop();
                int cd=dist[st][cv];
                for(auto it:g[cv])if(!used[it.second]){
                    dist[st][it.second]=cd+1;
                    que.push(it.second);
                    used[it.second]=true;
                }
            }
        }
        */
        for(int i=sz-1;i>=0;--i){
            if(g2[vlist[i]/N][vlist[i]%N]==' '){
                vlist.pop_back();
            }
            else break;
        }
        perm.clear();
        int mn=10000,id=-1;
        rep(i,vlist.size())if(vlist[i]/N==cy||vlist[i]%N==cx){
            int tmp=100*g[vlist[i]].size()+abs(vlist[i]/N-cy)+abs(vlist[i]%N-cx);
            if(mn>tmp)mn=tmp,id=vlist[i];
        }
        if(id==-1){
            mn=100000;
            rep(i,vlist.size()){
                int tmp=100*g[vlist[i]].size()+abs(vlist[i]/N-cy)+abs(vlist[i]%N-cx);
                if(mn>tmp)mn=tmp,id=vlist[i];
            }
        }
        if(id==cy*N+cx){
            int mx=0,td=0;
            rep(k,4){
                if(cy+dy[k]<0||cy+dy[k]>=N||cx+dx[k]<0||cx+dx[k]>=N||g1[cy+dy[k]][cx+dx[k]]=='x'){
                    mx=1,td=k;
                }
            }
            if(mx==0){
                int px2,py2;
                while(1){
                    py2=rnd.nextInt(N),px2=rnd.nextInt(N);
                    if(g1[py2][px2]=='x'){
                        break;
                    }
                }
                cout<<"P "<<py2<<" "<<px2<<" "<<cy+dy[0]<<" "<<cx+dx[0]<<endl;
                g1[py2][px2]='-',g1[cy+dy[0]][cx+dx[0]]='x';
                ++turn;
                td=0;
                if(turn>=M) break;
            }
            cout<<dir[td]<<endl;g2[cy][cx]=' ';
            ++turn;
            if(turn>=M) break;
        }
        else{
        pile.clear();
        int td=0,px2,py2;
        if(cy<id/N) td=1;
        bool edge=false,mv=false;
        if(id/N+dy[td]<0||id/N+dy[td]>=N||g1[id/N+dy[td]][cx]=='x') edge=true;
        while(cy!=id/N){
            cy+=dy[td],cx+=dx[td];
            mv=true;
            if(cy>=0&&cx>=0&&cy<N&&cx<N){
                if(g1[cy][cx]=='x'){
                    if(!edge){
                        py2=id/N+dy[td],px2=cx;
                        pile.pb(cy*N+cx,py2*N+px2);
                        edge=true;
                        g1[cy][cx]='-',g1[py2][px2]='x';
                    }
                    else{
                        while(1){
                            py2=rnd.nextInt(N),px2=rnd.nextInt(N);
                            if(px2!=cx&&py2!=cy&&py2!=id/N&&px2!=id%N&&g1[py2][px2]=='-'){
                                break;
                            }
                            
                        }
                        pile.pb(cy*N+cx,py2*N+px2);
                        g1[cy][cx]='-',g1[py2][px2]='x';
                    }
                }
            }
            else break;
        }
        if(!edge){
            while(1){
                py2=rnd.nextInt(N),px2=rnd.nextInt(N);
                if(px2!=cx&&py2!=cy&&py2!=id/N&&px2!=id%N&&g1[py2][px2]=='x'){
                    break;
                }
            }
            cout<<"P "<<py2<<" "<<px2<<" "<<id/N+dy[td]<<" "<<cx<<endl;
            g1[py2][px2]='-',g1[id/N+dy[td]][cx]='x';
            ++turn;
            if(turn>=M) break;
        }
        for(auto it:pile)if(turn<M){
            cout<<"P "<<it.first/N<<" "<<it.first%N<<" "<<it.second/N<<" "<<it.second%N<<endl;
            //g1[it.first/N][it.first%N]='-',g1[it.second/N][it.second%N]='x';
            ++turn;
            if(turn>=M) break;
        }
        
        if(turn>=M) break;
        if(mv){
            cout<<dir[td]<<endl;g2[cy][cx]=' ';
            ++turn;
        }
        if(turn>=M) break;
        
        pile.clear();
        edge=false,mv=false;
        td=2;
        if(cx<id%N) td=3;
        if(id%N+dx[td]<0||id%N+dx[td]>=N||g1[cy][id%N+dx[td]]=='x') edge=true;
        while(cx!=id%N){
            cy+=dy[td],cx+=dx[td];
            mv=true;
            if(cy>=0&&cx>=0&&cy<N&&cx<N){
                if(g1[cy][cx]=='x'){
                    if(!edge){
                        py2=id/N,px2=id%N+dx[td];
                        pile.pb(cy*N+cx,py2*N+px2);
                        edge=true;
                        g1[cy][cx]='-',g1[py2][px2]='x';
                    }
                    else{
                        while(1){
                            py2=rnd.nextInt(N),px2=rnd.nextInt(N);
                            if(px2!=cx&&py2!=cy&&py2!=id/N&&px2!=id%N&&g1[py2][px2]=='-'){
                                break;
                            }
                            
                        }
                        pile.pb(cy*N+cx,py2*N+px2);
                        g1[cy][cx]='-',g1[py2][px2]='x';
                    }
                }
            }
            else break;
        }
        if(!edge){
            while(1){
                py2=rnd.nextInt(N),px2=rnd.nextInt(N);
                if(px2!=cx&&py2!=cy&&py2!=id/N&&px2!=id%N&&g1[py2][px2]=='x'){
                    break;
                }
            }
            cout<<"P "<<py2<<" "<<px2<<" "<<cy<<" "<<id%N+dx[td]<<endl;
            g1[py2][px2]='-',g1[cy][id%N+dx[td]]='x';
            ++turn;
            if(turn>=M) break;
        }
        for(auto it:pile)if(turn<M){
            cout<<"P "<<it.first/N<<" "<<it.first%N<<" "<<it.second/N<<" "<<it.second%N<<endl;
            //g1[it.first/N][it.first%N]='-',g1[it.second/N][it.second%N]='x';
            ++turn;
            if(turn>=M) break;
        }
        
        if(turn>=M) break;
        if(mv){
            cout<<dir[td]<<endl;g2[cy][cx]=' ';
            ++turn;
        }
        if(turn>=M) break;
        }

        rep(i,vlist.size()){
            nei[vlist[i]].clear();
            rep(j,vlist.size())if(i!=j){
                if(dist[vlist[i]][vlist[j]]<=20) nei[vlist[i]].pb(vlist[j]);
            }
        }
        //tsp
        t_opt(id);
        //turn<M
        turn=ope_move(turn,id,rch);
        if(turn>=M) break;
        //select next char
        vector<int> ccnt(26);
        rep(j1,N)rep(j2,N)if(g2[j1][j2]!=' ') ++ccnt[g2[j1][j2]-'A'];
        rch=max_element(ccnt.begin(),ccnt.end())-ccnt.begin();
        int mxx=0;
        rep(i,cpos[rch].size()){
            int tmp=ufv[rch].size(cpos[rch][i]);
            if(mxx<tmp)mxx=tmp,rpoint=cpos[rch][i];
        }
    }
    return 0;
}