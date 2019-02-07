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
    return (w=(w^(w>>19))^(t^(t>>8)));
}

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
const int dx[]={1,0,-1,0},dy[]={0,1,0,-1};
inline bool isdigit(char d){return d>='0'&&d<='9';}
class CrystalLighting {
public:
    Timer timer;
    int H,W,W2;
    int cur_score,best_score;
    int co_count[8],twi[3]={3,5,6};
    int cost_lan,cost_mir,cost_obs,max_mir,max_obs;
    int obs_count,mir_count;
    vector<string> G,best_state;
    int ray_info[103][103],co_info[103][103][4];
    vector<vector<int> > lan_info,lan_best;
    int tmpco[4];
    vector<pint> cand;
    struct Crystal{
        int8_t init,cur,point,cup,pos[4];
        bool f;
        Crystal(){}
        Crystal(int8_t init):init(init),cur(init),f(true),cup(0){
            point=((init-1)&init)?30:20;
        }
        inline void update(){
            cur=init^(pos[0]|pos[1]|pos[2]|pos[3]);
            f=(init|cur)==init?true:false;
            cup=cur==0?point:cur==init?0:-10;
        }
    };
    Crystal cry[11001];
    // 1:'/' 2:'\'
    int cnv_m1[4]={3,2,1,0},cnv_m2[4]={1,0,3,2};//m1:^=3,m2:^=1
    void ems_move(int ty,int tx,int co,int dir){
        int cb=1<<dir;
        while(G[ty][tx]!='Z'){
                if(G[ty][tx]=='.') ray_info[ty][tx]|=cb,co_info[ty][tx][dir]=co;
                else if(G[ty][tx]=='/'){
                    dir^=3,cb=1<<dir,ray_info[ty][tx]|=cb,co_info[ty][tx][dir]=co;
                }
                else if(G[ty][tx]=='\\'){
                    dir^=1,cb=1<<dir,ray_info[ty][tx]|=cb,co_info[ty][tx][dir]=co;
                }
                else break;
                ty-=dy[dir],tx-=dx[dir];
        }
        if(isdigit(G[ty][tx])){
            int id=ty*W2+tx,pre=cry[id].cup;
            cry[id].pos[dir]=co;
            cry[id].update();
            cur_score+=cry[id].cup-pre;
        }
        return;
    }
    void abs_move(int ty,int tx,int dir){
        int cb=1<<dir;
        while(G[ty][tx]!='Z'){
                if(G[ty][tx]=='.') ray_info[ty][tx]&=~cb,co_info[ty][tx][dir]=0;
                else if(G[ty][tx]=='/'){
                    dir^=3,cb=1<<dir,ray_info[ty][tx]&=~cb,co_info[ty][tx][dir]=0;
                }
                else if(G[ty][tx]=='\\'){
                    dir^=1,cb=1<<dir,ray_info[ty][tx]&=~cb,co_info[ty][tx][dir]=0;
                }
                else break;
                ty-=dy[dir],tx-=dx[dir];
        }
        if(isdigit(G[ty][tx])){
            int id=ty*W2+tx,pre=cry[id].cup;
            cry[id].pos[dir]=0;
            cry[id].update();
            cur_score+=cry[id].cup-pre;
        }
        return;
    }
    void put_lan(int ty,int tx,int cco){
        cur_score-=cost_lan;
        lan_info[ty][tx]=cco;
        //rep(i,4) ems_move(ty,tx,cco,i);
        ems_move(ty,tx,cco,0);
        ems_move(ty,tx,cco,1);
        ems_move(ty,tx,cco,2);
        ems_move(ty,tx,cco,3);
        G[ty][tx]='L';
        return;
    }
    void remove_lan(int ty,int tx){
        G[ty][tx]='.';
        lan_info[ty][tx]=0;
        cur_score+=cost_lan;
        //rep(i,4) abs_move(ty,tx,i);
        abs_move(ty,tx,0);
        abs_move(ty,tx,1);
        abs_move(ty,tx,2);
        abs_move(ty,tx,3);
        return;
    }
    void put_obs(int ty,int tx){
        ++obs_count;
        cur_score-=cost_obs;
        //rep(i,4) abs_move(ty,tx,i);
        abs_move(ty,tx,0);
        abs_move(ty,tx,1);
        abs_move(ty,tx,2);
        abs_move(ty,tx,3);
        G[ty][tx]='O';
        return;
    }
    void remove_obs(int ty,int tx){
        int cco;
        --obs_count;
        G[ty][tx]='.';
        cur_score+=cost_obs;
        int y=ty,x=tx-1;
        if(co_info[y][x][2]!=0){
            cco=co_info[y][x][2];x=tx;
            ems_move(y,x,cco,2);
        }
        y=ty-1,x=tx;
        if(co_info[y][x][3]!=0){
            cco=co_info[y][x][3];y=ty;
            ems_move(y,x,cco,3);
        }
        y=ty,x=tx+1;
        if(co_info[y][x][0]!=0){
            cco=co_info[y][x][0];x=tx;
            ems_move(y,x,cco,0);
        }
        y=ty+1,x=tx;
        if(co_info[y][x][1]!=0){
            cco=co_info[y][x][1];y=ty;
            ems_move(y,x,cco,1);
        }
        return;
    }
    // '/'
    void put_mir1(int ty,int tx){
        cur_score-=cost_mir;
        ++mir_count;
        int ray_dir=ray_info[ty][tx];
        //rep(i,4)tmpco[i]=co_info[ty][tx][i];
        tmpco[0]=co_info[ty][tx][0];
        tmpco[1]=co_info[ty][tx][1];
        tmpco[2]=co_info[ty][tx][2];
        tmpco[3]=co_info[ty][tx][3];
        rep(i,4)if(ray_dir>>i&1){
            int cnvd=cnv_m1[i],cco=tmpco[i];
            if(ray_dir!=9&&ray_dir!=6){
                abs_move(ty,tx,i);
            }
            ems_move(ty,tx,cco,cnvd);
        }
        G[ty][tx]='/';
        return;
    }
    // '/'
    void remove_mir1(int ty,int tx){
        cur_score+=cost_mir;
        --mir_count;
        G[ty][tx]='.';
        int ray_dir=0;
        rep(i,4)if(G[ty+dy[i]][tx+dx[i]]!='Z'&&(ray_info[ty+dy[i]][tx+dx[i]]>>i&1)){
            ray_dir|=(1<<i);
        }
        rep(i,4)if(ray_dir>>i&1){
            int cnvd=cnv_m1[i],co=co_info[ty+dy[i]][tx+dx[i]][i];
            ems_move(ty,tx,co,i);
            if(ray_dir!=9&&ray_dir!=6){
                abs_move(ty,tx,cnvd);
            }
        }
        return;
    }
    void put_mir2(int ty,int tx){
        cur_score-=cost_mir;
        ++mir_count;
        int ray_dir=ray_info[ty][tx];
        //rep(i,4)tmpco[i]=co_info[ty][tx][i];
        tmpco[0]=co_info[ty][tx][0];
        tmpco[1]=co_info[ty][tx][1];
        tmpco[2]=co_info[ty][tx][2];
        tmpco[3]=co_info[ty][tx][3];
        rep(i,4)if(ray_dir>>i&1){
            int cnvd=cnv_m2[i],cco=tmpco[i];
            if(ray_dir!=3&&ray_dir!=12){
                abs_move(ty,tx,i);
            }
            ems_move(ty,tx,cco,cnvd);
        }
        G[ty][tx]='\\';
        return;
    }
    void remove_mir2(int ty,int tx){
        cur_score+=cost_mir;
        --mir_count;
        G[ty][tx]='.';
        int ray_dir=0;
        rep(i,4)if(G[ty+dy[i]][tx+dx[i]]!='Z'&&(ray_info[ty+dy[i]][tx+dx[i]]>>i&1)){
            ray_dir|=(1<<i);
        }
        rep(i,4)if(ray_dir>>i&1){
            int cnvd=cnv_m2[i],co=co_info[ty+dy[i]][tx+dx[i]][i];
            ems_move(ty,tx,co,i);
            if(ray_dir!=3&&ray_dir!=12){
                abs_move(ty,tx,cnvd);
            }
        }
        return;
    }
    void erase_lan(pair<pint,char> tg){
        int ty=tg.first.first,tx=tg.first.second;
        char co=tg.second;
        int y=ty,x=tx-1;
        while(G[y][x]=='.') --x;
        if(isdigit(G[y][x])){
            if(cry[y*W2+x].cur==0){
                return;
            }
        }
        y=ty-1,x=tx;
        while(G[y][x]=='.') --y;
        if(isdigit(G[y][x])){
            if(cry[y*W2+x].cur==0){
                return;
            }
        }
        y=ty,x=tx+1;
        while(G[y][x]=='.') ++x;
        if(isdigit(G[y][x])){
            if(cry[y*W2+x].cur==0){
                return;
            }
        }
        y=ty+1,x=tx;
        while(G[y][x]=='.') ++y;
        if(isdigit(G[y][x])){
            if(cry[y*W2+x].cur==0){
                return;
            }
        }
        remove_lan(ty,tx);
        return;
    }
    void update_best(int cur){
        best_score=cur;
        best_state=G;
        lan_best=lan_info;
        //cerr<<"update time: "<<timer.get()<<endl;
    }
    inline bool forceupdate(double sub,double temp){
        if(sub>=0) return true;
        double d=(sub)*temp;
        if(d<-6) return false;
        return exp(d)>(double)(xor128()&1048575)/1048575;
    }
    void improve(){
        //double start=timer.get();
        int sz=H*W;
        double starttemp=7.5,endtemp=1.5,curtemp;
        int turn=-1;
        double invtl=1.0/timeLimit;
        while(1){
            ++turn;
            double t=timer.get();
            if(t>timeLimit){
                //cerr<<turn<<endl;
                return;
            }
            curtemp=1.0/(starttemp+(endtemp-starttemp)*t*invtl);
            int pre_score=cur_score;
            int id,ix,iy;
            while(1){
                id=xor128()%sz;
                iy=id/W+1,ix=id%W+1;
                if(isdigit(G[iy][ix])||G[iy][ix]=='L'||G[iy][ix]=='O'||G[iy][ix]=='\\'||G[iy][ix]=='/')break;
            }
            id=iy*W2+ix;
            if(G[iy][ix]=='O'){
                int ray_dir=0;
                rep(i,4)if(G[iy+dy[i]][ix+dx[i]]!='Z'){
                    if(ray_info[iy+dy[i]][ix+dx[i]]>>i&1) ray_dir|=(1<<i);
                }
                if(ray_dir==0){
                    remove_obs(iy,ix);
                    if(!forceupdate(cur_score-pre_score,curtemp)){
                        put_obs(iy,ix);
                    }
                    else if(best_score<cur_score){
                        update_best(cur_score);
                    }
                }
                else if(((turn&1)||mir_count==max_mir)&&(ray_dir&5)!=5&&(ray_dir&10)!=10){
                    int dir=ray_dir;
                    if(ray_dir&(ray_dir-1)){
                        dir=(xor128()&1)?ray_dir&-ray_dir:ray_dir&(ray_dir-1);
                    }
                    dir=__builtin_ctz(dir);
                    if(xor128()&1) dir=(dir+2)&3;
                    int y=iy+dy[dir],x=ix+dx[dir];
                    cand.clear();
                    while(G[y][x]=='.'){
                        if(ray_info[y][x]!=0)cand.pb(y,x);
                        y+=dy[dir],x+=dx[dir];
                    }
                    int cnt=cand.size();
                    if(cnt!=0){
                        int mov=xor128()%cnt;
                        y=cand[mov].first,x=cand[mov].second;
                        remove_obs(iy,ix);
                        put_obs(y,x);
                        if(!forceupdate(cur_score-pre_score,curtemp)){
                            remove_obs(y,x);
                            put_obs(iy,ix);
                        }
                        else if(best_score<cur_score){
                            update_best(cur_score);
                        }
                    }
                }
                else if(mir_count<max_mir&&(ray_dir&3)!=3&&(ray_dir&12)!=12){
                    remove_obs(iy,ix);
                    put_mir1(iy,ix);
                    if(!forceupdate(cur_score-pre_score,curtemp)){
                        remove_mir1(iy,ix);
                        put_obs(iy,ix);
                    }
                    else if(best_score<cur_score){
                        update_best(cur_score);
                    }
                }
                else if(mir_count<max_mir&&(ray_dir&6)!=6&&(ray_dir&9)!=9){
                    remove_obs(iy,ix);
                    put_mir2(iy,ix);
                    if(!forceupdate(cur_score-pre_score,curtemp)){
                        remove_mir2(iy,ix);
                        put_obs(iy,ix);
                    }
                    else if(best_score<cur_score){
                        update_best(cur_score);
                    }
                }
            }
            else if(G[iy][ix]=='/'){
                if((ray_info[iy][ix]&5)==5||(ray_info[iy][ix]&10)==10){
                    if(obs_count<max_obs){
                        remove_mir1(iy,ix);
                        put_obs(iy,ix);
                        if(!forceupdate(cur_score-pre_score,curtemp)){
                            remove_obs(iy,ix);
                            put_mir1(iy,ix);
                        }
                        else if(best_score<cur_score){
                            update_best(cur_score);
                        }
                    }
                    else continue;
                }
                else if(ray_info[iy][ix]==0||(turn&1)){
                //else{
                    remove_mir1(iy,ix);
                    if(!forceupdate(cur_score-pre_score,curtemp)){
                        put_mir1(iy,ix);
                    }
                    else if(best_score<cur_score){
                        update_best(cur_score);
                    }
                }
                else{
                    int dir=ray_info[iy][ix];
                    if(ray_info[iy][ix]&(ray_info[iy][ix]-1)){
                        dir=(xor128()&1)?(ray_info[iy][ix]&-ray_info[iy][ix]):ray_info[iy][ix]&(ray_info[iy][ix]-1);
                    }
                    dir=__builtin_ctz(dir);
                    int bit=(dir&1)?10:5;
                    if(xor128()&1) dir=(dir+2)&3;
                    int y=iy+dy[dir],x=ix+dx[dir];
                    cand.clear();
                    while(G[y][x]=='.'){
                        if(((ray_info[y][x]|bit)&3)!=3&&((ray_info[y][x]|bit)&12)!=12)cand.pb(y,x);
                        y+=dy[dir],x+=dx[dir];
                    }
                    int cnt=cand.size();
                    if(cnt!=0){
                        int mov=xor128()%cnt;
                        y=cand[mov].first,x=cand[mov].second;
                        remove_mir1(iy,ix);
                        put_mir1(y,x);
                        if(!forceupdate(cur_score-pre_score,curtemp)){
                            remove_mir1(y,x);
                            put_mir1(iy,ix);
                        }
                        else if(best_score<cur_score){
                            update_best(cur_score);
                        }
                    }
                }
            }
            else if(G[iy][ix]=='\\'){
                if((ray_info[iy][ix]&5)==5||(ray_info[iy][ix]&10)==10){
                    if(obs_count<max_obs){
                        remove_mir2(iy,ix);
                        put_obs(iy,ix);
                        if(!forceupdate(cur_score-pre_score,curtemp)){
                            remove_obs(iy,ix);
                            put_mir2(iy,ix);
                        }
                        else if(best_score<cur_score){
                            update_best(cur_score);
                        }
                    }
                    else continue;
                }
                else if(ray_info[iy][ix]==0||(turn&1)){
                //else{
                    remove_mir2(iy,ix);
                    if(!forceupdate(cur_score-pre_score,curtemp)){
                        put_mir2(iy,ix);
                    }
                    else if(best_score<cur_score){
                        update_best(cur_score);
                    }
                }
                else{
                    int dir=ray_info[iy][ix];
                    if(ray_info[iy][ix]&(ray_info[iy][ix]-1)){
                        dir=(xor128()&1)?(ray_info[iy][ix]&-ray_info[iy][ix]):ray_info[iy][ix]&(ray_info[iy][ix]-1);
                    }
                    dir=__builtin_ctz(dir);
                    if(xor128()&1) dir=(dir+2)&3;
                    int y=iy+dy[dir],x=ix+dx[dir];
                    int bit=(dir&1)?10:5;
                    cand.clear();
                    while(G[y][x]=='.'){
                        if(((ray_info[y][x]|bit)&6)!=6&&((ray_info[y][x]|bit)&9)!=9)cand.pb(y,x);
                        y+=dy[dir],x+=dx[dir];
                    }
                    int cnt=cand.size();
                    if(cnt!=0){
                        int mov=xor128()%cnt;
                        y=cand[mov].first,x=cand[mov].second;
                        remove_mir2(iy,ix);
                        put_mir2(y,x);
                        if(!forceupdate(cur_score-pre_score,curtemp)){
                            remove_mir2(y,x);
                            put_mir2(iy,ix);
                        }
                        else if(best_score<cur_score){
                            update_best(cur_score);
                        }
                    }
                }
            }
            else if(G[iy][ix]=='L'){
                int co=lan_info[iy][ix];
                if(turn&1){
                    remove_lan(iy,ix);
                    if(!forceupdate(cur_score-pre_score,curtemp)){
                        put_lan(iy,ix,co);
                    }
                    else if(best_score<cur_score){
                        update_best(cur_score);
                    }
                }
                else{
                    int dir=xor128()&3;
                    int y=iy+dy[dir],x=ix+dx[dir];
                    cand.clear();
                    while(G[y][x]=='.'){
                        if((ray_info[y][x]&(ray_info[y][x]-1))==0)cand.pb(y,x);
                        y+=dy[dir],x+=dx[dir];
                    }
                    int cnt=cand.size();
                    if(cnt!=0){
                        int mov=xor128()%cnt;
                        y=cand[mov].first,x=cand[mov].second;
                        remove_lan(iy,ix);
                        put_lan(y,x,co);
                        if(!forceupdate(cur_score-pre_score,curtemp)){
                            remove_lan(y,x);
                            put_lan(iy,ix,co);
                        }
                        else if(best_score<cur_score){
                            update_best(cur_score);
                        }
                    }
                }
            }
            else {
                int pos=xor128()&3;
                int ty=iy,tx=ix;
                bool flag=xor128()&1;
                cand.clear();
                if(cry[id].pos[pos]==0){
                    if(!cry[id].f) continue;
                    if(cry[id].cur==0){
                        int co=cry[id].init;
                        if(co&(co-1)){
                            if(flag) co=co&-co;
                            else co=co&(co-1);
                        }
                        int y=ty+dy[pos],x=tx+dx[pos];
                        while(G[y][x]=='.'){
                            if(ray_info[y][x]==0)cand.pb(y,x);
                            y+=dy[pos],x+=dx[pos];
                        }
                        int cnt=cand.size();
                        if(cnt!=0){
                            int mov=xor128()%cnt;
                            y=cand[mov].first,x=cand[mov].second;
                            put_lan(y,x,co);
                            if(!forceupdate(cur_score-pre_score,curtemp)){
                                remove_lan(y,x);
                            }
                            else if(best_score<cur_score){
                                update_best(cur_score);
                            }
                        }
                    }
                    else{
                        int co=cry[id].cur;
                        double fac=1;
                        if(co&(co-1)){
                            if(flag) co=co&-co;
                            else co=co&(co-1);
                            fac=0.5;
                        }
                        int y=ty+dy[pos],x=tx+dx[pos];
                        if((turn&1)||mir_count==max_mir){
                            while(G[y][x]=='.'){
                                if(ray_info[y][x]==0)cand.pb(y,x);
                                y+=dy[pos],x+=dx[pos];
                            }
                            int cnt=cand.size();
                            if(cnt!=0){
                                int mov=xor128()%cnt;
                                y=cand[mov].first,x=cand[mov].second;
                                put_lan(y,x,co);
                                if(!forceupdate(fac*(cur_score-pre_score),curtemp)){
                                    remove_lan(y,x);
                                }
                                else if(best_score<cur_score){
                                    update_best(cur_score);
                                }
                            }
                        }
                        else if(mir_count<max_mir){
                            while(G[y][x]=='.'){
                                if(co_info[y][x][cnv_m1[pos]]==co){
                                    put_mir1(y,x);
                                    if(!forceupdate(fac*(cur_score-pre_score),curtemp)){
                                        remove_mir1(y,x);
                                    }
                                    else if(best_score<cur_score){
                                        update_best(cur_score);
                                    }
                                    break;
                                }
                                else if(co_info[y][x][cnv_m2[pos]]==co){
                                    put_mir2(y,x);
                                    if(!forceupdate(fac*(cur_score-pre_score),curtemp)){
                                        remove_mir2(y,x);
                                    }
                                    else if(best_score<cur_score){
                                        update_best(cur_score);
                                    }
                                    break;
                                }
                                y+=dy[pos],x+=dx[pos];
                            }
                        }
                    }
                }
                else{
                    if(!cry[id].f){
                        int y=ty+dy[pos],x=tx+dx[pos];
                        int choice=xor128()&3;
                        while(G[y][x]=='.'){
                            if(choice==0&&(ray_info[y][x]&(ray_info[y][x]-1))==0)cand.pb(y,x);
                            else if(choice==1&&(ray_info[y][x]&3)!=3&&(ray_info[y][x]&12)!=12)cand.pb(y,x);
                            else if(choice==2&&(ray_info[y][x]&6)!=6&&(ray_info[y][x]&9)!=9)cand.pb(y,x);
                            y+=dy[pos],x+=dx[pos];
                        }
                        int cnt=cand.size();
                        int co=co_info[y][x][pos];
                        bool op=!cry[id].f&&(((co^cry[id].cur)|cry[id].init)==cry[id].init);
                        if(op&&obs_count<max_obs&&cnt!=0&&choice==0){
                            int mov=xor128()%cnt;
                            y=cand[mov].first,x=cand[mov].second;
                            put_obs(y,x);
                            if(!forceupdate(cur_score-pre_score,curtemp)){
                                remove_obs(y,x);
                            }
                            else if(best_score<cur_score){
                                update_best(cur_score);
                            }
                        }
                        else if(op&&mir_count<max_mir&&cnt!=0&&choice==1){
                            int mov=xor128()%cnt;
                            y=cand[mov].first,x=cand[mov].second;
                            put_mir1(y,x);
                            if(!forceupdate(cur_score-pre_score,curtemp)){
                                remove_mir1(y,x);
                            }
                            else if(best_score<cur_score){
                                update_best(cur_score);
                            }
                        }
                        else if(op&&mir_count<max_mir&&cnt!=0&&choice==2){
                            int mov=xor128()%cnt;
                            y=cand[mov].first,x=cand[mov].second;
                            put_mir2(y,x);
                            if(!forceupdate(cur_score-pre_score,curtemp)){
                                remove_mir2(y,x);
                            }
                            else if(best_score<cur_score){
                                update_best(cur_score);
                            }
                        }
                        else{
                            if(lan_info[y][x]!=0){
                                remove_lan(y,x);
                                if(!forceupdate(cur_score-pre_score,curtemp)){
                                    put_lan(y,x,co);
                                }
                                else if(best_score<cur_score){
                                    update_best(cur_score);
                                }
                            }
                            else if(G[y][x]=='/'&&(ray_info[y][x]&5)!=5&&(ray_info[y][x]&10)!=10){
                                remove_mir1(y,x);
                                if(!forceupdate(cur_score-pre_score,curtemp)){
                                    put_mir1(y,x);
                                }
                                else if(best_score<cur_score){
                                    update_best(cur_score);
                                }
                            }
                            else if(G[y][x]=='\\'&&(ray_info[y][x]&5)!=5&&(ray_info[y][x]&10)!=10){
                                remove_mir2(y,x);
                                if(!forceupdate(cur_score-pre_score,curtemp)){
                                    put_mir2(y,x);
                                }
                                else if(best_score<cur_score){
                                    update_best(cur_score);
                                }
                            }
                        } 
                    }
                    else{
                        int y=ty+dy[pos],x=tx+dx[pos];
                        int lx=-1,ly;
                        while((G[y][x]=='.'||G[y][x]=='L')){
                            if(G[y][x]=='L') ly=y,lx=x;
                            else if((ray_info[y][x]&(ray_info[y][x]-1))==0)cand.pb(y,x);
                            y+=dy[pos],x+=dx[pos];
                        }
                        int cnt=cand.size();
                        if(lx==-1) continue;
                        if(cnt!=0){
                            int co=lan_info[ly][lx];
                            int mov=xor128()%cnt;
                            y=cand[mov].first,x=cand[mov].second;
                            remove_lan(ly,lx);
                            put_lan(y,x,co);
                            if(!forceupdate(cur_score-pre_score,curtemp)){
                                remove_lan(y,x);
                                put_lan(ly,lx,co);
                            }
                            else if(best_score<cur_score){
                                update_best(cur_score);
                            }
                        }
                    }
                }
            }
        }
        return;
    }
    
    
    vector<string> placeItems(vector<string> targetBoard, int costLantern, int costMirror, int costObstacle, int maxMirrors, int maxObstacles) {
        cost_lan=costLantern,cost_mir=costMirror,cost_obs=costObstacle;
        max_mir=maxMirrors,max_obs=maxObstacles;
        timer.reset();
        H=targetBoard.size(),W=targetBoard[0].size();
        W2=W+2;
        G=vector<string>(H+2,string(W+2,'Z'));
        //int count=0;
        FOR(i,1,H+1)FOR(j,1,W+1){
            G[i][j]=targetBoard[i-1][j-1];
            if(isdigit(targetBoard[i-1][j-1])) cry[i*W2+j]=Crystal(G[i][j]-'0');//++count;
        }
        lan_info=vector<vector<int> >(H+2,vector<int>(W+2,0));
        //cerr<<H<<" "<<W<<" "<<count<<endl;
        vector<int> zero;
        FOR(i,1,H+1){
            int score=0,ty,tx,cco;
            FOR(j,1,W+1){
                if(G[i][j]=='.'){
                    memset(co_count,0,sizeof(co_count));
                    zero.clear();
                    int y=i,x=j-1;
                    while(G[y][x]=='.') --x;
                    if(isdigit(G[y][x])&&cry[y*W2+x].f){
                        if(cry[y*W2+x].cur==0) zero.pb(cry[y*W2+x].init);
                        else ++co_count[cry[y*W2+x].cur];
                    }
                    else if(G[y][x]=='L') continue;
                    y=i-1,x=j;
                    while(G[y][x]=='.') --y;
                    if(isdigit(G[y][x])&&cry[y*W2+x].f){
                        if(cry[y*W2+x].cur==0) zero.pb(cry[y*W2+x].init);
                        else ++co_count[cry[y*W2+x].cur];
                    }
                    else if(G[y][x]=='L') continue;
                    y=i,x=j+1;
                    while(G[y][x]=='.') ++x;
                    if(isdigit(G[y][x])&&cry[y*W2+x].f){
                        if(cry[y*W2+x].cur==0) zero.pb(cry[y*W2+x].init);
                        else ++co_count[cry[y*W2+x].cur];
                    }
                    else if(G[y][x]=='L') continue;
                    y=i+1,x=j;
                    while(G[y][x]=='.') ++y;
                    if(isdigit(G[y][x])&&cry[y*W2+x].f){
                        if(cry[y*W2+x].cur==0) zero.pb(cry[y*W2+x].init);
                        else ++co_count[cry[y*W2+x].cur];
                    }
                    else if(G[y][x]=='L') continue;
                    int co_id=-1,cur=0;
                    for(int k=1;k<=4;k<<=1){
                        if(cur<co_count[k]) cur=co_count[k]*5,co_id=k;
                    }
                    if(co_id==-1){
                        rep(k,3)if(cur<co_count[twi[k]]) cur=co_count[twi[k]],co_id=twi[k];
                        if(co_id!=-1)(co_id&1)?co_id^=1:co_id^=2;
                        else continue;
                    }
                    FOR(k,1,7)if(co_id!=k){
                        if((co_id|k)==k) cur+=co_count[k];
                        else cur-=5*co_count[k];
                    }
                    for(auto it:zero)if((co_id|it)!=it) cur-=5;
                    if(score<cur) score=cur,ty=i,tx=j,cco=co_id;
                }
                else{
                    if(score>0){
                        put_lan(ty,tx,cco);
                    }
                    score=0;
                }
            }
            if(score>0){
                put_lan(ty,tx,cco);
            }
            score=0;
        }
        update_best(cur_score);
        improve();
        vector<string> ans;
        cerr<<best_score<<endl;
        char obj;
        FOR(i,1,H+1)FOR(j,1,W+1){
            if(best_state[i][j]=='L'){
                obj='0'+lan_best[i][j];
                ans.pb(to_string(i-1)+" "+to_string(j-1)+" "+obj);
            }
            else if(best_state[i][j]=='O'){
                obj='X';
                ans.pb(to_string(i-1)+" "+to_string(j-1)+" "+obj);
            }
            else if(best_state[i][j]=='/'){
                obj='/';
                ans.pb(to_string(i-1)+" "+to_string(j-1)+" "+obj);
            }
            else if(best_state[i][j]=='\\'){
                obj='\\';
                ans.pb(to_string(i-1)+" "+to_string(j-1)+" "+obj);
            }
        }
        
        //for(auto it:ans) cerr<<it<<endl;
        return ans;
    }
};
// -------8<------- end of solution submitted to the website -------8<-------

template<class T> void getVector(vector<T>& v) {
    for (int i = 0; i < v.size(); ++i)
        cin >> v[i];
}

int main() {
    CrystalLighting cl;
    int H;
    cin >> H;
    vector<string> targetBoard(H);
    getVector(targetBoard);
    int costLantern, costMirror, costObstacle, maxMirrors, maxObstacles;
    cin >> costLantern >> costMirror >> costObstacle >> maxMirrors >> maxObstacles;

    vector<string> ret = cl.placeItems(targetBoard, costLantern, costMirror, costObstacle, maxMirrors, maxObstacles);
    cout << ret.size() << endl;
    for (int i = 0; i < (int)ret.size(); ++i)
        cout << ret[i] << endl;
    cout.flush();
}
