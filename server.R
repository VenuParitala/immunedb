A<-read.csv("Data/table1.csv")
B<-read.csv("Data/abstract-covid19-set_mind.csv")
C<-read.csv("Data/csv-covid19-set.csv")
D<-read.csv("Data/innative.csv")
E<-read.csv("Data/auto.csv")
F<-read.csv("Data/innativedis.csv")
G<-read.csv("Data/autodis.csv")
H<-read.csv("Data/innavtivecon.csv")
I<-read.csv("Data/autocon.csv")
J<-read.csv("Data/abstract-Chronicgra-set_mind.csv")
K<-read.csv("Data/abstract-PrimaryImm-set_mind.csv")
L<-read.csv("Data/abstract-complement-set_mind.csv")
M<-read.csv("Data/abstract-HyperIgESy-set_mind.csv")
N<-read.csv("Data/abstract-Innateimmu-set_mind.csv")
O<-read.csv("Data/abstract-NEMODefici-set_mind.csv")
P<-read.csv("Data/abstract-type1diabe-set_mind.csv")
Q<-read.csv("Data/abstract-rheumatoid-set_mind.csv")
R<-read.csv("Data/abstract-psorisisar-set_mind.csv")
S<-read.csv("Data/abstract-multiplesc-set_mind.csv")
T<-read.csv("Data/abstract-Systemiclu-set_mind.csv")
U<-read.csv("Data/abstract-Inflammato-set.csv")
V<-read.csv("Data/summary-adhisondis-set_mind.csv")
W<-read.csv("Data/abstract-gravesdise-set_mind.csv")
X<-read.csv("Data/abstract-sjogrensyn-set_mind.csv")
Y<-read.csv("Data/abstract-hashimotot-set_mind.csv")
Z<-read.csv("Data/abstract-myasthenia-set_mind.csv")
a<-read.csv("Data/abstract-autoimmune-set_mind.csv")
b<-read.csv("Data/abstract-pernicious-set_mind.csv")
c<-read.csv("Data/abstract-celiacdise-set_mind.csv")
d<-read.csv("Data/csv-Chronicgra-set.csv")
e<-read.csv("Data/csv-PrimaryImm-set.csv")
f<-read.csv("Data/csv-complement-set.csv")
g<-read.csv("Data/csv-HyperIgESy-set.csv")
h<-read.csv("Data/csv-InnateImmu-set.csv")
i<-read.csv("Data/csv-NEMODefici-set.csv")
j<-read.csv("Data/csv-Type1diabe-set.csv")
k<-read.csv("Data/csv-Rheumatoid-set.csv")
l<-read.csv("Data/csv-Psoriasisp-set.csv")
m<-read.csv("Data/csv-Multiplesc-set.csv")
n<-read.csv("Data/csv-Systemiclu-set.csv")
o<-read.csv("Data/csv-Inflammato-set.csv")
p<-read.csv("Data/csv-Addisonsdi-set.csv")
q<-read.csv("Data/csv-Gravesdise-set.csv")
r<-read.csv("Data/csv-Sjogrenssy-set.csv")
s<-read.csv("Data/csv-Hashimotos-set.csv")
t<-read.csv("Data/csv-Myasthenia-set.csv")
u<-read.csv("Data/csv-Autoimmune-set.csv")
v<-read.csv("Data/csv-Pernicious-set.csv")
w<-read.csv("Data/csv-Celiacdise-set.csv")
AA<-read.csv("Data/adaptivedis.csv")
BB<-read.csv("Data/adaptivetype.csv")
CC<-read.csv("Data/adaptivecon.csv")
DD<-read.csv("Data/abstract-Rehumaticd-set_mind.csv")
EE<-read.csv("Data/csv-Rehumaticd-set.csv")
FF<-read.csv("Data/drug.csv")
GG<-read.csv("Data/food.csv")
HH<-read.csv("Data/age.csv")
aa<-read.csv("Data/Complement Deficiencies.csv")
bb<-read.csv("Data/Primary immune deficiency.csv")
cc<-read.csv("Data/chronic granulomatous.csv")
dd<-read.csv("Data/Hyper IgE Syndrome.csv")
ee<-read.csv("Data/Innate Immune Defects.csv")
ff<-read.csv("Data/NEMO Deficiency Syndrome.csv")
gg<-read.csv("Data/Addisons disease.csv")
hh<-read.csv("Data/Celiac disease.csv")
ii<-read.csv("Data/Graves.csv")
jj<-read.csv("Data/Autoimmune vasculitis.csv")
kk<-read.csv("Data/Hashimotos thyroiditis.csv")
ll<-read.csv("Data/Inflammatory bowel disease.csv")
mm<-read.csv("Data/Multiple sclerosis.csv")
nn<-read.csv("Data/Myasthenia gravis.csv")
oo<-read.csv("Data/Psoriasis or psoriatic arthritis.csv")
pp<-read.csv("Data/Pernicious anemia.csv")
qq<-read.csv("Data/Sjogrens syndrome.csv")
rr<-read.csv("Data/Systemic lupus erythematosus.csv")
ss<-read.csv("Data/type 1 diabetes.csv")
tt<-read.csv("Data/Rheumatoid arthritis.csv")
uu<-read.csv("Data/Rheumatic diseases.csv")
server<-function(input,output){
  output$table1<-renderDataTable({
    A<-as.data.frame(A)
    datatable(A)
  })
  
  output$table2<-renderDataTable({
    B<-as.data.frame(B)
    datatable(B,filter = "top")
  })
  output$plot777<-renderPlot({
    B<-as.data.frame(B)
    B<-head(B,25)
    ggplot(B,aes(B$Gene_symbol,B$Freq))+geom_bar(stat = "identity",fill="blue")+
      labs(title = "The plot represent by nuber articles present genes",x="gene symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$table3<-renderDataTable({
    C<-as.data.frame(C)
    datatable(C,filter = "top")
  })
  output$plot888<-renderPlot({
    C<-as.data.frame(C)
    C<-head(C,30)
    ggplot(C,aes(C$PMID,C$Title))+geom_bar(stat = "identity",fill="green")+
      labs(title = "The plot represent by pubmed id and title",x="pubmed id",y="title")+theme(axis.text.x = element_text(angle = 90))
  })
  output$plot999<-renderPlot({
    C<-as.data.frame(C)
    C<-head(C,30)
    ggplot(C,aes(C$PMID,C$First.Author))+geom_bar(stat = "identity",fill="yellow")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$plot1000<-renderPlot({
    C<-as.data.frame(C)
    C<-head(C,30)
    ggplot(C,aes(C$First.Author,C$Create.Date))+geom_bar(stat = "identity",fill="steelblue")+
      labs(title = "The plot represent by author and submission date",x="first author",y="date of submission")+theme(axis.text.x = element_text(angle = 90))
  })
  output$tab1<-renderDataTable({
    D<-as.data.frame(D)
    datatable(D,filter = "top")
  })
  output$plot111<-renderPlot({
    D<-as.data.frame(D)
    D<-head(D)
    ggplot(D,aes(D$DISEASE.NAME,D$DISEASE.CATAGORIES))+geom_bar(stat = "identity",fill="green")+
      labs(title = "The plot represent by disease name and categorie",x="Disease name",y="Disease categire")+theme(axis.text.x = element_text(angle = 90))
  })

  output$tab2<-renderDataTable({
    E<-as.data.frame(E)
    datatable(E,filter = "top")
  })
  output$plot222<-renderPlot({
    E<-as.data.frame(E)
    E<-head(E,20)
    ggplot(E,aes(E$DISEASE.NAME,E$DISEASE.CATAGORY))+geom_bar(stat = "identity",fill="blue")+
      labs(title = "The plot represent by disease name and categorie",x="Disease name",y="Disease categire")+theme(axis.text.x = element_text(angle = 90))
  })
  output$tab4<-renderDataTable({
    F<-as.data.frame(F)
    datatable(F,filter = "top")
  })
  output$plot444<-renderPlot({
    F<-as.data.frame(F)
    F<-head(F)
    ggplot(F,aes(F$DISEASE.NAME,F$DISEASE.TYPE))+geom_bar(stat = "identity",fill="blue")+
      labs(title = "The plot represent by disease name and DISEASE TYPE",x="Disease name",y="Disease type")+theme(axis.text.x = element_text(angle = 90))
  })
  output$netplot1<-renderPlot({
    F<-as.data.frame(F)
    F<-ggnetwork(F,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
    ggplot()+
      geom_edges(data = F,
                 aes(x=x,y=y,xend=xend,yend=yend),
                 color="steelblue",curvature = 0.1,size=0.15,alpha=1/2)+
      geom_nodes(data = F,
                 aes(x=x,y=y,xend=xend,yend=yend,size=0.15),
                 alpha=1/2,color="orange")+
      theme_blank()+theme(legend.position = "none")+labs(title = "Disease name and disease type Related network")
  })
  output$tab5<-renderDataTable({
    G<-as.data.frame(G)
    datatable(G,filter = "top")
  })
  output$plot555<-renderPlot({
    G<-as.data.frame(G)
    G<-head(G)
    ggplot(G,aes(G$DISEASE.NAME,G$DISEASE.TYPE))+geom_bar(stat = "identity",fill="pink")+
      labs(title = "The plot represent by disease name and DISEASE TYPE",x="Disease name",y="Disease type")+theme(axis.text.x = element_text(angle = 90))
  })
  output$netplot2<-renderPlot({
    G<-as.data.frame(G)
    G<-ggnetwork(G,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
    ggplot()+
      geom_edges(data = G,
                 aes(x=x,y=y,xend=xend,yend=yend),
                 color="steelblue",curvature = 0.2,size=0.20,alpha=1/3)+
      geom_nodes(data = G,
                 aes(x=x,y=y,xend=xend,yend=yend,size=0.20),
                 alpha=1/2,color="green")+
     
      theme_blank()+theme(legend.position = "none")+labs(title = "Disease name and disease type Related network")
  })
  output$tab7<-renderDataTable({
    H<-as.data.frame(H)
    datatable(H,filter = "top")
  })
  output$tab8<-renderDataTable({
    I<-as.data.frame(I)
    datatable(I,filter = "top")
  })
  output$gene1<-renderDataTable({
    J<-as.data.frame(J)
    datatable(J,filter = "top")
  })
  output$plot1<-renderPlot({
    J<-as.data.frame(J)
    J<-head(J,20)
    ggplot(J,aes(J$Gene_symbol,J$Freq))+geom_bar(stat = "identity",fill="green")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
 
  output$gene2<-renderDataTable({
    K<-as.data.frame(K)
    datatable(K,filter = "top")
  })
  output$plot2<-renderPlot({
    K<-as.data.frame(K)
    K<-head(K,20)
    ggplot(K,aes(K$Gene_symbol,K$Freq))+geom_bar(stat = "identity",fill="orange")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$gene3<-renderDataTable({
    L<-as.data.frame(L)
    datatable(L,filter = "top")
  })
  output$plot3<-renderPlot({
    L<-as.data.frame(L)
    L<-head(L,20)
    ggplot(L,aes(L$Gene_symbol,L$Freq))+geom_bar(stat = "identity",fill="blue")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$gene4<-renderDataTable({
    M<-as.data.frame(M)
    datatable(M,filter = "top")
  })
  output$plot4<-renderPlot({
    M<-as.data.frame(M)
    M<-head(M,20)
    ggplot(M,aes(M$Gene_symbol,M$Freq))+geom_bar(stat = "identity",fill="red")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$gene5<-renderDataTable({
    N<-as.data.frame(N)
    datatable(N,filter = "top")
  })
  output$plot5<-renderPlot({
    N<-as.data.frame(N)
    N<-head(N,20)
    ggplot(N,aes(N$Gene_symbol,N$Freq))+geom_bar(stat = "identity",fill="black")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$gene6<-renderDataTable({
    O<-as.data.frame(O)
    datatable(O,filter = "top")
    
  })
  output$plot6<-renderPlot({
    O<-as.data.frame(O)
    O<-head(O,20)
    ggplot(O,aes(O$Gene_symbol,O$Freq))+geom_bar(stat = "identity",fill="green")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$gene11<-renderDataTable({
    P<-as.data.frame(P)
    datatable(P,filter = "top")
  })
  output$plot7<-renderPlot({
    P<-as.data.frame(P)
    P<-head(P,20)
    ggplot(P,aes(P$Gene_symbol,P$Freq))+geom_bar(stat = "identity",fill="steelblue")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$gene22<-renderDataTable({
    Q<-as.data.frame(Q)
    datatable(Q,filter = "top")
  })
  output$plot8<-renderPlot({
    Q<-as.data.frame(Q)
    Q<-head(Q,20)
    ggplot(Q,aes(Q$Gene_symbol,Q$Freq))+geom_bar(stat = "identity",fill="pink")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$gene33<-renderDataTable({
    R<-as.data.frame(R)
    datatable(R,filter = "top")
  })
  output$plot9<-renderPlot({
    R<-as.data.frame(R)
    R<-head(R,20)
    ggplot(R,aes(R$Gene_symbol,R$Freq))+geom_bar(stat = "identity",fill="black")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$gene44<-renderDataTable({
    S<-as.data.frame(S)
    datatable(S,filter = "top")
  })
  output$plot10<-renderPlot({
    S<-as.data.frame(S)
    S<-head(S,20)
    ggplot(S,aes(S$Gene_symbol,S$Freq))+geom_bar(stat = "identity",fill="red")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$gene55<-renderDataTable({
    T<-as.data.frame(T)
    datatable(T,filter = "top")
  })
  output$plot11<-renderPlot({
    T<-as.data.frame(T)
    T<-head(T,20)
    ggplot(T,aes(T$Gene_symbol,T$Freq))+geom_bar(stat = "identity",fill="green")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$gene66<-renderDataTable({
    U<-as.data.frame(U)
    datatable(U,filter = "top")
  })
  output$plot12<-renderPlot({
    U<-as.data.frame(U)
    U<-head(U,20)
    ggplot(U,aes(U$Gene_symbol,U$Freq))+geom_bar(stat = "identity",fill="red")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$gene77<-renderDataTable({
    V<-as.data.frame(V)
    datatable(V,filter = "top")
  })
  output$plot13<-renderPlot({
    V<-as.data.frame(V)
    V<-head(V,20)
    ggplot(V,aes(V$Gene_symbol,V$Freq))+geom_bar(stat = "identity",fill="blue")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$gene88<-renderDataTable({
    W<-as.data.frame(W)
    datatable(W,filter = "top")
  })
  output$plot14<-renderPlot({
    W<-as.data.frame(W)
    W<-head(W,20)
    ggplot(W,aes(W$Gene_symbol,W$Freq))+geom_bar(stat = "identity",fill="black")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$gene99<-renderDataTable({
    X<-as.data.frame(X)
    datatable(X,filter = "top")
  })
  output$plot15<-renderPlot({
    X<-as.data.frame(X)
    X<-head(X,20)
    ggplot(X,aes(X$Gene_symbol,X$Freq))+geom_bar(stat = "identity",fill="green")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$gene10<-renderDataTable({
    Y<-as.data.frame(Y)
    datatable(Y,filter = "top")
  })
  output$plot16<-renderPlot({
    Y<-as.data.frame(Y)
    Y<-head(Y,20)
    ggplot(Y,aes(Y$Gene_symbol,Y$Freq))+geom_bar(stat = "identity",fill="pink")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$gene111<-renderDataTable({
    Z<-as.data.frame(Z)
    datatable(Z,filter = "top")
  })
  output$plot17<-renderPlot({
    Z<-as.data.frame(Z)
    Z<-head(Z,20)
    ggplot(Z,aes(Z$Gene_symbol,Z$Freq))+geom_bar(stat = "identity",fill="blue")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$gene12<-renderDataTable({
    a<-as.data.frame(a)
    datatable(a,filter = "top")
  })
  output$plot18<-renderPlot({
    a<-as.data.frame(a)
    a<-head(a,20)
    ggplot(a,aes(a$Gene_symbol,a$Freq))+geom_bar(stat = "identity",fill="yellow")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$gene13<-renderDataTable({
    b<-as.data.frame(b)
    datatable(b,filter = "top")
  })
  output$plot19<-renderPlot({
    b<-as.data.frame(b)
    b<-head(b,20)
    ggplot(b,aes(b$Gene_symbol,b$Freq))+geom_bar(stat = "identity",fill="steelblue")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$gene14<-renderDataTable({
    c<-as.data.frame(c)
    datatable(c,filter = "top")
  })
  output$plot20<-renderPlot({
    c<-as.data.frame(c)
    c<-head(c,20)
    ggplot(c,aes(c$Gene_symbol,c$Freq))+geom_bar(stat = "identity",fill="yellow")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs1<-renderDataTable({
    d<-as.data.frame(d)
    datatable(d,filter = "top")
  })
  output$graph1<-renderPlot({
    d<-as.data.frame(d)
    d<-head(d,20)
    ggplot(d,aes(d$PMID,d$First.Author))+geom_bar(stat = "identity",fill="yellow")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs2<-renderDataTable({
    e<-as.data.frame(e)
    datatable(e,filter = "top")
  })
  output$graph2<-renderPlot({
    e<-as.data.frame(e)
    e<-head(e,20)
    ggplot(e,aes(e$PMID,e$First.Author))+geom_bar(stat = "identity",fill="green")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs3<-renderDataTable({
    f<-as.data.frame(f)
    datatable(f,filter = "top")
  })
  output$graph3<-renderPlot({
    f<-as.data.frame(f)
    f<-head(f,20)
    ggplot(f,aes(f$PMID,f$First.Author))+geom_bar(stat = "identity",fill="blue")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs4<-renderDataTable({
    g<-as.data.frame(g)
    datatable(g,filter = "top")
  })
  output$graph4<-renderPlot({
    g<-as.data.frame(g)
    g<-head(g,20)
    ggplot(g,aes(g$PMID,g$First.Author))+geom_bar(stat = "identity",fill="black")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs5<-renderDataTable({
    h<-as.data.frame(h)
    datatable(h,filter = "top")
  })
  output$graph5<-renderPlot({
    h<-as.data.frame(h)
    h<-head(h,20)
    ggplot(h,aes(h$PMID,h$First.Author))+geom_bar(stat = "identity",fill="pink")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs6<-renderDataTable({
    i<-as.data.frame(i)
    datatable(i,filter = "top")
  })
  output$graph6<-renderPlot({
    i<-as.data.frame(i)
    i<-head(i,20)
    ggplot(i,aes(i$PMID,i$First.Author))+geom_bar(stat = "identity",fill="yellow")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs11<-renderDataTable({
    j<-as.data.frame(j)
    datatable(j,filter = "top")
  })
  output$graph7<-renderPlot({
    j<-as.data.frame(j)
    j<-head(j,20)
    ggplot(j,aes(j$PMID,j$First.Author))+geom_bar(stat = "identity",fill="blue")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs22<-renderDataTable({
    k<-as.data.frame(k)
    datatable(k,filter = "top")
  })
  output$graph8<-renderPlot({
    k<-as.data.frame(k)
    k<-head(k,20)
    ggplot(k,aes(k$PMID,k$First.Author))+geom_bar(stat = "identity",fill="orange")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs33<-renderDataTable({
    l<-as.data.frame(l)
    datatable(l,filter = "top")
  })
  output$graph9<-renderPlot({
    l<-as.data.frame(l)
    l<-head(l,20)
    ggplot(l,aes(l$PMID,l$First.Author))+geom_bar(stat = "identity",fill="steelblue")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs44<-renderDataTable({
    m<-as.data.frame(m)
    datatable(m,filter = "top")
  })
  output$graph10<-renderPlot({
    m<-as.data.frame(m)
    m<-head(m,20)
    ggplot(m,aes(m$PMID,m$First.Author))+geom_bar(stat = "identity",fill="red")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs55<-renderDataTable({
    n<-as.data.frame(n)
    datatable(n,filter = "top")
  })
  output$graph11<-renderPlot({
    n<-as.data.frame(n)
    n<-head(n,20)
    ggplot(n,aes(n$PMID,n$First.Author))+geom_bar(stat = "identity",fill="yellow")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs66<-renderDataTable({
    o<-as.data.frame(o)
    datatable(o,filter = "top")
    
  })
  output$graph12<-renderPlot({
    o<-as.data.frame(o)
    o<-head(o,20)
    ggplot(o,aes(o$PMID,o$First.Author))+geom_bar(stat = "identity",fill="green")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs77<-renderDataTable({
    p<-as.data.frame(p)
    datatable(p,filter = "top")
  })
  output$graph13<-renderPlot({
    p<-as.data.frame(p)
    p<-head(p,20)
    ggplot(p,aes(p$PMID,p$First.Author))+geom_bar(stat = "identity",fill="blue")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs88<-renderDataTable({
    q<-as.data.frame(q)
    datatable(q,filter = "top")
  })
  output$graph14<-renderPlot({
    q<-as.data.frame(q)
    q<-head(q,20)
    ggplot(q,aes(q$PMID,q$First.Author))+geom_bar(stat = "identity",fill="yellow")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs99<-renderDataTable({
    r<-as.data.frame(r)
    datatable(r,filter = "top")
  })
  output$graph15<-renderPlot({
    r<-as.data.frame(r)
    r<-head(r,20)
    ggplot(r,aes(r$PMID,r$First.Author))+geom_bar(stat = "identity",fill="steelblue")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs10<-renderDataTable({
    s<-as.data.frame(s)
    datatable(s,filter = "top")
  })
  output$graph16<-renderPlot({
    s<-as.data.frame(s)
    s<-head(s,20)
    ggplot(s,aes(s$PMID,s$First.Author))+geom_bar(stat = "identity",fill="green")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs111<-renderDataTable({
    t<-as.data.frame(t)
    datatable(t,filter = "top")
  })
  output$graph17<-renderPlot({
    t<-as.data.frame(t)
    t<-head(t,20)
    ggplot(t,aes(t$PMID,t$First.Author))+geom_bar(stat = "identity",fill="black")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs12<-renderDataTable({
      u<-as.data.frame(u)
      datatable(u,filter = "top")
    })
  output$graph18<-renderPlot({
    u<-as.data.frame(u)
    u<-head(u,20)
    ggplot(u,aes(u$PMID,u$First.Author))+geom_bar(stat = "identity",fill="red")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs13<-renderDataTable({
    v<-as.data.frame(v)
    datatable(v,filter = "top")
  })
  output$graph19<-renderPlot({
    v<-as.data.frame(v)
    v<-head(v,20)
    ggplot(v,aes(v$PMID,v$First.Author))+geom_bar(stat = "identity",fill="blue")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs14<-renderDataTable({
    w<-as.data.frame(w)
    datatable(w,filter = "top")
  })
  output$graph20<-renderPlot({
    w<-as.data.frame(w)
    w<-head(w,20)
    ggplot(w,aes(w$PMID,w$First.Author))+geom_bar(stat = "identity",fill="yellow")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$tab3<-renderDataTable({
    AA<-as.data.frame(AA)
    datatable(AA)
  })
  output$plot333<-renderPlot({
    AA<-as.data.frame(AA)
    AA<-head(AA)
    ggplot(AA,aes(AA$Disease.name,AA$Disease.category))+geom_bar(stat = "identity",fill="orange")+
      labs(title = "The graph represent by disease name and disease catagories",x="disease name",y="disease catagorie")+theme(axis.text.x = element_text(angle = 90))
  })
  
  output$tab6<-renderDataTable({
    BB<-as.data.frame(BB)
    datatable(BB)
  })
  output$plot666<-renderPlot({
    BB<-as.data.frame(BB)
    BB<-head(BB)
    ggplot(BB,aes(BB$Disease.name,BB$Disease.Type))+geom_bar(stat = "identity",fill="yellow")+
      labs(title = "The plot represent by disease name and DISEASE TYPE",x="Disease name",y="Disease type")+theme(axis.text.x = element_text(angle = 90))
  })
  output$netplot3<-renderPlot({
    BB<-as.data.frame(BB)
    BB<-ggnetwork(BB,layout="fruchtermanreingold",arrow.gap=0,cell.jitter=0)
    ggplot()+
      geom_edges(data = BB,
                 aes(x=x,y=y,xend=xend,yend=yend),
                 color="steelblue",curvature = 0.2,size=0.20,alpha=1/3)+
      geom_nodes(data = BB,
                 aes(x=x,y=y,xend=xend,yend=yend,size=0.20),
                 alpha=1/2,color="green")+
      
      theme_blank()+theme(legend.position = "none")+labs(title = "Disease name and disease type Related network")
  })
  output$tab9<-renderDataTable({
    CC<-as.data.frame(CC)
    datatable(CC)
  })
  output$gene15<-renderDataTable({
    DD<-as.data.frame(DD)
    datatable(DD)
  })
  output$plot21<-renderPlot({
    DD<-as.data.frame(DD)
    DD<-head(DD,20)
    ggplot(DD,aes(DD$Gene_symbol,DD$Freq))+geom_bar(stat = "identity",fill="green")+
      labs(title = "The graph represent by top genes used in number of articles",x="gene_symbol",y="freq")+theme(axis.text.x = element_text(angle = 90))
  })
  output$abs15<-renderDataTable({
    EE<-as.data.frame(EE)
    datatable(EE)
  })
  output$graph21<-renderPlot({
    EE<-as.data.frame(EE)
    EE<-head(EE,20)
    ggplot(EE,aes(EE$PMID,EE$First.Author))+geom_bar(stat = "identity",fill="blue")+
      labs(title = "The plot represent by pubmed id and first author",x="pubmed id",y="first author")+theme(axis.text.x = element_text(angle = 90))
  })
  output$drug<-renderDataTable({
    FF<-as.data.frame(FF)
    datatable(FF,filter = "top")
  })
  output$food<-renderDataTable({
    GG<-as.data.frame(GG)
    datatable(GG,filter = "top")
  })
  output$foodplot<-renderPlot({
    GG<-as.data.frame(GG)
    GG<-head(GG)
    ggplot(GG,aes(GG$Food.name,GG$vitamin))+geom_bar(stat = "identity",fill="blue")+
      labs(title = "The plot represent by food name and vitamins",x="food name",y="vitamins")+theme(axis.text.x = element_text(angle = 90))
  })
  output$foodplot1<-renderPlot({
    GG<-as.data.frame(GG)
    GG<-head(GG)
    ggplot(GG,aes(GG$Food.name,GG$Food.Category))+geom_bar(stat = "identity",fill="orange")+
      labs(title = "The plot represent by food name and vitamins",x="food name",y="food catagory")+theme(axis.text.x = element_text(angle = 90))
  })
  output$age<-renderDataTable({
    HH<-as.data.frame(HH)
    datatable(HH)
  })
  output$ageplot1<-renderPlot({
    HH<-as.data.frame(HH)
    HH<-head(HH)
    ggplot(HH,aes(HH$Age.of.a.person,HH$Percentage.of.causing.immune.deficiency))+geom_bar(stat = "identity",fill="blue")+
      labs(title = "The plot represent by age of the person and causing deficiency",x="age of the person",y="causing deficiency")+theme(axis.text.x = element_text(angle = 90))
  })
  output$ageplot2<-renderPlot({
    HH<-as.data.frame(HH)
    HH<-head(HH)
    ggplot(HH,aes(HH$Age.of.a.person,HH$percentage.to.cure.the.disease))+geom_bar(stat = "identity",fill="green")+
      labs(title = "The plot represent by age of the person  and disease cure",x="age of the person",y="disease cure")+theme(axis.text.x = element_text(angle = 90))
  })
  output$ageplot3<-renderPlot({
    HH<-as.data.frame(HH)
    HH<-head(HH)
    ggplot(HH,aes(HH$Percentage.of.causing.immune.deficiency,HH$percentage.to.cure.the.disease))+geom_bar(stat = "identity",fill="red")+
      labs(title = "The plot represent by Percentage.of.causing.immune.deficiency and percentage.to.cure.the.disease",x="Percentage.of.causing.immune.deficiency",y="percentage.to.cure.the.disease")+theme(axis.text.x = element_text(angle = 90))
  })
 output$targ1<-renderDataTable({
   aa<-as.data.frame(aa)
   datatable(aa)
 })
 output$targ2<-renderDataTable({
   bb<-as.data.frame(bb)
   datatable(bb)
 })
 output$targ3<-renderDataTable({
   cc<-as.data.frame(cc)
   datatable(cc)
 })
 output$targ4<-renderDataTable({
   dd<-as.data.frame(dd)
   datatable(dd)
 })
 output$targ5<-renderDataTable({
   ee<-as.data.frame(ee)
   datatable(ee)
 })
 output$targ6<-renderDataTable({
   ff<-as.data.frame(ff)
   datatable(ff)
 })
 output$targ7<-renderDataTable({
   ss<-as.data.frame(ss)
   datatable(ss)
 })
 output$targ8<-renderDataTable({
   tt<-as.data.frame(tt)
   datatable(tt)
 })
 output$targ9<-renderDataTable({
   oo<-as.data.frame(oo)
   datatable(oo)
 })
 output$targ10<-renderDataTable({
   mm<-as.data.frame(mm)
   datatable(mm)
 })
 output$targ11<-renderDataTable({
   rr<-as.data.frame(rr)
   datatable(rr)
 })
 output$targ12<-renderDataTable({
   ll<-as.data.frame(ll)
   datatable(ll)
 })
 output$targ13<-renderDataTable({
   gg<-as.data.frame(gg)
   datatable(gg)
 })
 output$targ14<-renderDataTable({
   ii<-as.data.frame(ii)
   datatable(ii)
 })
 output$targ15<-renderDataTable({
   qq<-as.data.frame(qq)
   datatable(qq)
 })
 output$targ16<-renderDataTable({
   kk<-as.data.frame(kk)
   datatable(kk)
 })
 output$targ17<-renderDataTable({
   nn<-as.data.frame(nn)
   datatable(nn)
 })
 output$targ18<-renderDataTable({
   jj<-as.data.frame(jj)
   datatable(jj)
 })
 output$targ19<-renderDataTable({
   pp<-as.data.frame(pp)
   datatable(pp)
 })
 output$targ20<-renderDataTable({
   hh<-as.data.frame(hh)
   datatable(hh)
 })
 output$targ21<-renderDataTable({
   uu<-as.data.frame(uu)
   datatable(uu)
 })
}