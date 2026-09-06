/* Independent design renderer. It never imports the Emacs implementation. */
(function (root) {
  'use strict';
  const palette = {
    base: ['#d8dee9','#15171c'], muted: ['#8d929e','#15171c'], orange: ['#e9b96e','#15171c'],
    purple: ['#c792ea','#15171c'], blue: ['#8cc4ff','#15171c'], green: ['#8ae234','#15171c'],
    inactiveGreen: ['#52882f','#15171c'], botShortcut: ['#dfb8f5','#211a2b'], managerShortcut: ['#f5d39a','#292218'],
    botInput: ['#f5f7fb','#211b29'], managerInput: ['#f5f7fb','#272119'],
    internal: ['#a9aeba','#20232b'], internalBadge: ['#c2c6cf','#383b44'], metadataValue: ['#eeeeee','#292c34'],
    errorMessage: ['#f07886','#2b1c21'], error: ['#f07886','#15171c'], rule: ['#414550','#15171c'],
    managerBadge: ['#ffffff','#976b32'], botBadge: ['#ffffff','#754993'], location: ['#eeeeee','#383b44'],
    user: ['#f5f7fb','#19232c'], userBadge: ['#ffffff','#527da6'], manager: ['#f5f7fb','#272119'], bot: ['#f5f7fb','#211b29'],
    purpleBar: ['#c792ea','#211a2b'], orangeBar: ['#e9b96e','#292218'],
    selected: ['#d8dee9','#24262d'], selectedTitle: ['#e9b96e','#24262d'], selectedMuted: ['#a0a4af','#24262d'],
    key: ['#eeeeee','#343740'], cursor: ['#15171c','#f0d29c'], selection: ['#ffffff','#48648c'],
    searchInput: ['#eeeeee','#383b44'],
    code: ['#d1d7e0','#20232b'], link: ['#8cc4ff','#15171c'], echo: ['#a9aeba','#1c1e25'],
  };
  const chars = text => [...String(text ?? '')];
  const width = char => /[\u0300-\u036f\ufe00-\ufe0f]/u.test(char) ? 0 :
    /[\u1100-\u115f\u2e80-\ua4cf\uac00-\ud7a3\uf900-\ufaff\uff01-\uff60\u{1f300}-\u{1faff}]/u.test(char) ? 2 : 1;
  const textWidth = text => chars(text).reduce((sum,ch) => sum + width(ch),0);
  function clip(text, columns, ellipsis = false) {
    const source = chars(text); let result = '', used = 0;
    const limit = ellipsis && textWidth(text) > columns ? columns - 1 : columns;
    for (const ch of source) { if (used + width(ch) > limit) break; result += ch; used += width(ch); }
    return result + (ellipsis && textWidth(text) > columns ? '…' : '');
  }
  function wrap(text, columns) {
    return String(text ?? '').replace(/\t/g,'        ').split('\n').flatMap(line => {
      const result = []; let rest = line;
      while (textWidth(rest) > columns) {
        let part = clip(rest,columns); const space = part.lastIndexOf(' ');
        if (space > columns / 2) part = part.slice(0,space);
        result.push(part); rest = rest.slice(part.length).replace(/^ /,'');
      }
      result.push(rest); return result;
    });
  }
  function inputLines(text, columns, point = String(text).length) {
    const lines = ['']; let row = 0, col = 0, offset = 0, cursor = {row:0,col:0};
    for (const ch of chars(text)) {
      if (offset === point) cursor = {row,col};
      if (ch === '\n') { lines.push(''); row++; col = 0; }
      else { if (col + width(ch) > columns) { lines.push(''); row++; col = 0; }
        lines[row] += ch; col += width(ch);
        if (col === columns) { lines.push(''); row++; col = 0; }
      }
      offset += ch.length;
    }
    if (point >= offset) cursor = {row,col};
    return {lines,cursor};
  }
  function searchResults(state) {
    if(state.searchLoading || state.noResults)return [];
    const words=(state.query||'').toLowerCase().split(/\s+/).filter(Boolean);
    return (state.results||state.chats||[]).filter(chat=>chat.kind!=='shadow' && words.every(word=>{
      const label=(chat.label+' '+chat.title).toLowerCase();let offset=0;
      for(const char of word){offset=label.indexOf(char,offset);if(offset<0)return false;offset++;}
      return true;
    }));
  }
  function render(state, columns = 120, rows = 40) {
    const grid = Array.from({length:rows},()=>Array.from({length:columns},()=>({ch:' ',face:'base'})));
    const regions = {}; let cursor = null;
    const put = (x,y,text,face='base',action=null) => {
      let col = x;
      for (const ch of chars(text)) {
        const size = width(ch);
        if (y >= 0 && y < rows && col >= 0 && col + size <= columns) {
          if (!size) { if (col > 0) grid[y][col-1].ch += ch; }
          else { grid[y][col] = {ch,face,...(action ? {action} : {})};
            if (size === 2) grid[y][col+1] = {ch:'',face,...(action ? {action} : {})}; }
        }
        col += size;
      }
      return col;
    };
    const fill = (x,y,w,h,face) => {
      for(let yy=Math.max(0,y);yy<Math.min(rows,y+h);yy++)
        for(let xx=Math.max(0,x);xx<Math.min(columns,x+w);xx++) grid[yy][xx]={ch:' ',face};
    };
    const withBackground = (face, background) => {
      const name = face + 'On' + background;
      palette[name] = [palette[face][0], palette[background][1]];
      return name;
    };
    const statusFace = chat => chat.active ? 'green' : ['error','interrupted','cancelled','blocked'].includes(chat.status) ? 'error' : 'muted';
    const badge = (x,y,chat,withLocation=true,background='base',integrated=false) => {
      const roleFace=chat.kind==='bot'?'botBadge':'managerBadge';
      let col=x;
      if(integrated) {
        col=put(col,y,' ',roleFace);col=put(col,y,'●',withBackground(statusFace(chat),roleFace));
        col=put(col,y,` ${chat.label} `,roleFace);
      } else {
        col=put(col,y,'●',withBackground(statusFace(chat),background));col=put(col,y,' ',background);
        col=put(col,y,` ${chat.label} `,roleFace);
      }
      if(withLocation && chat.kind === 'bot' && chat.location) col=put(col,y,` ${chat.location} `,'location');
      return col;
    };
    const visibleChats = (state.chats || []).filter(chat=>chat.kind!=='shadow');
    const chatById = id => visibleChats.find(chat=>chat.id===id);
    const target = chatById(state.target);
    const current = chatById(state.current);
    const text = state.draft || '';
    const draftLayout = inputLines(text,columns-2,state.point ?? text.length);
    const attached = state.attachments || [];
    const attachmentRows = attached.length ? attached.length + 1 : 0;
    const inputRows = Math.min(10,Math.max(1,draftLayout.lines.length),Math.max(1,Math.floor(rows/2)-3-attachmentRows));
    const composerVisible = state.mode!=='cowork' || state.inputOpen;
    const composerHeight = composerVisible ? 3 + inputRows + attachmentRows : 0;
    const contentHeight = rows - 1 - composerHeight;
    const sidebarWidth = state.mode==='cowork'?0:35;
    const mainX = sidebarWidth ? sidebarWidth + 1 : 0;
    const mainWidth = columns-mainX;
    regions.main = {x:mainX,y:0,width:mainWidth,height:contentHeight};
    if(sidebarWidth) {
      regions.sidebar={x:0,y:0,width:35,height:contentHeight};
      for(let y=0;y<contentHeight;y++) put(35,y,'│','rule');
      const output=[]; let index=0;
      const section=(title,items,isRead=false)=>{
        output.push({heading:`# (${items.length}) ${title}`});
        output.push({});
        for(const item of (isRead?items.slice(0,5):items)) {
          const chat=chatById(item.chat || item.id); if(!chat) continue;
          const selected=chat.id===state.current;
          if(title==='ACTIVE') {
            output.push({chat,item,index,selected,part:'active'}); index++;continue;
          }
          output.push({chat,item,index,selected,part:'badge'});
          output.push({chat,item,selected,part:'title'});
          for(const line of wrap(item.body || 'Working…',30)) output.push({chat,item,selected,part:'body',line});
          output.push({}); index++;
        }
        if(Object.keys(output.at(-1)).length)output.push({});
      };
      section('ACTIVE',visibleChats.filter(chat=>chat.active));
      section('UNREAD',(state.unread||[]).filter(item=>chatById(item.chat)));
      section('READ',(state.read||[]).filter(item=>chatById(item.chat)),true);
      const offset=0;
      output.slice(offset,offset+contentHeight).forEach((line,y)=>{
        if(line.heading) {put(1,y,line.heading,'orange');return;}
        if(line.rule){put(1,y,'─'.repeat(33),'rule');return;}
        if(!line.chat)return;
        if(line.selected && line.part!=='active'){fill(3,y,32,1,'selected');put(3,y,'▏','selectedTitle');}
        const action=`chat:${line.chat.id}`;
        if(line.part==='active') {
          put(1,y,line.index<16?line.index.toString(16):' ','base',action);
          if(line.selected)put(3,y,'▏','orange',action);
          const end=badge(4,y,line.chat,true,'base',true);
          put(end+1,y,clip((line.chat.project?line.chat.project+' · ':'')+line.chat.title,33-end,true),'muted',action);
        } else if(line.part==='badge') {
          put(1,y,line.index<16?line.index.toString(16):' ','muted',action);
          const end=badge(4,y,line.chat,false,line.selected?'selected':'base');
          const time=line.item.time||line.chat.time||'2m'; put(Math.max(end+1,34-textWidth(time)),y,time,line.selected?'selectedMuted':'muted',action);
        } else put(4,y,clip(line.part==='title'?line.chat.title:line.line,30,true),
                   line.part==='title'?(line.selected?'selectedTitle':'orange'):(line.selected?'selectedMuted':'muted'),action);
        for(let x=1;x<35;x++)grid[y][x].action=action;
      });
    }
    function pageLines(page,chat,width) {
      const lines=[];
      const line=(text='',face='base',action=null)=>lines.push({text,face,action});
      const heading=text=>{line(text,'orange');line();};
      const chatrow=chat=>lines.push({chat,row:true});
      if(page==='home') {
        visibleChats.filter(chat=>!chat.project).slice(0,10).forEach(chatrow);
        (state.projects||[]).filter(p=>!p.archived).sort((a,b)=>b.name.localeCompare(a.name)).forEach(project=>{
          line(); heading(`# ${project.name}`);
          visibleChats.filter(chat=>chat.project===project.name).slice(0,10).forEach(chatrow);
        });
        const archived=(state.projects||[]).filter(p=>p.archived);
        if(archived.length)line();
        archived.forEach(p=>line(p.name,'muted'));
        if(!visibleChats.length){lines.length=0;line('No conversations yet','muted');}
      } else if(page==='help') {
        heading('# Help');
        const bindings=[['C-M-h','Home / return to full mode'],['M-0','New m# chat'],['M-1 … M-9','Open b# from the purple bar'],['C-c b 0 … 9','Open m# from the orange bar'],['C-c b','Search chats (cowork mode)'],['C-c p s','Search chats in either mode'],['C-c n 0 … z','Open a notification'],['C-c p n','Focus and scroll notifications'],['C-c p l','Locations'],['C-c p p','Projects'],['C-c p h','Help'],['M-/','Focus input; repeat to cycle targets'],['C-o','Cycle content / input; skip sidebar'],['RET','Newline in input; activate links/buttons'],['M-RET','Send draft'],['C-g','Clear; also close input in cowork'],['C-c C-i','Attach clipboard image'],['M-w','Copy literal selected text'],['C-x 0/1/2/3','Leave full mode; normal window command']];
        bindings.forEach(([key,description])=>lines.push({key,description}));
      } else if(page==='locations') {
        heading('# Locations');
        line('NAME            WORKING DIRECTORY','muted');line();
        (state.locations||[]).forEach(location=>{
          line(`${location.name.padEnd(16)}${location.path}`);
        });
        if(!state.locations?.length)line('No locations configured.','muted');
      } else if(page==='projects') {
        heading('# Projects');
        (state.projects||[]).filter(p=>!p.archived).forEach(project=>{
          line(project.name,'orange');
          wrap(project.description,width-4).forEach(text=>line('  '+text));
          line(`  ~/.cache/aibo/projects/${project.name}/README.md`,'link','scenario:project-readme');line();
        });
        const archived=(state.projects||[]).filter(p=>p.archived);
        if(archived.length){if(lines.at(-1)?.text)line();heading('# Archived');archived.forEach(project=>line(project.name,'muted'));}
        if(!state.projects?.length)line('No projects configured.','muted');
      } else if(page==='project-readme') {
        state.file.content.split('\n').forEach(text=>line(text));
      } else if(page==='file') {
        state.file.content.split('\n').forEach(text=>line(text));
      } else if(page==='chat' && chat) {
        const messageLines=message=>{
          const item=message.data?.item;
          const type=item?.type||message.content;
          const internal=!['user','assistant'].includes(message.kind);
          if(message.kind==='user')lines.push({userBadge:true});
          else if(internal)lines.push({internalBadge:type==='reasoning'?'reasoning':type==='commandExecution'?'exec':type==='fileChange'?'file change':message.kind[0].toUpperCase()+message.kind.slice(1)});
          else lines.push({chat});
          if(internal && type==='reasoning'){line();return;}
          const face=internal?'internal':message.kind==='user'?'user':chat.kind==='bot'?'bot':'manager';
          if(internal && ['commandExecution','fileChange'].includes(type)) {
            const content=type==='commandExecution'?(item?.command||'Command unavailable'):
              (item?.changes||[]).map(change=>`${change.kind?.type||'update'} ${change.path}${change.kind?.movePath?' → '+change.kind.movePath:''}\n${change.diff||''}`).join('\n');
            for(const part of wrap(content,width-4))lines.push({text:part,cells:chars(part).map(ch=>({ch,face})),face,role:face,message:true});
            line();return;
          }
          let fence=null;
          for(let text of message.content.split('\n')) {
            const marker=text.match(/^\s*(`{3,}|~{3,})(.*)$/);
            if(marker && (!fence || (marker[1][0]===fence[0] && marker[1].length>=fence.length && !marker[2].trim()))) {
              fence=fence?null:marker[1];continue;
            }
            const rowFace=fence?'code':face;
            let textFace=rowFace;
            if(!fence && /^#{1,6}\s+/.test(text)) {
              text=text.replace(/^#{1,6}\s+/,'');textFace=withBackground('orange',face);
            }
            const cells=[];
            const append=(text,face,action=null)=>chars(text).forEach(ch=>cells.push({ch,face,action}));
            if(fence)append(text,'code');
            else {
              const tokens=/\[([^\]]+)\]\(([^)]+)\)|`([^`]+)`|\*\*([^*]+)\*\*/g;
              let offset=0;
              for(const match of text.matchAll(tokens)) {
                append(text.slice(offset,match.index),textFace);
                if(match[1])append(match[1],withBackground('blue',face),'scenario:link-text');
                else append(match[3]||match[4],match[3]?'key':textFace);
                offset=match.index+match[0].length;
              }
              append(text.slice(offset),textFace);
            }
            let offset=0;
            for(const part of wrap(cells.map(cell=>cell.ch).join(''),width-4)) {
              const count=chars(part).length;
              lines.push({text:part,cells:cells.slice(offset,offset+count),face:rowFace,role:face,message:true});
              offset+=count;if(cells[offset]?.ch===' ')offset++;
            }
          }
          line();
        };
        for(const message of (state.messages||[])) {
          if(!['user','assistant'].includes(message.kind)) {
            if(message.groupStart!==false)line(`[${message.count||1} hidden message${message.count===1?'':'s'}] ${state.expanded?'▾':'▸'}`,'muted','hidden:toggle');
            line();
            if(state.expanded)(message.messages||[message]).forEach(messageLines);
          } else messageLines(message);
        }
        if(state.notice){wrap(state.notice,width-4).forEach(text=>line(text,chat.status==='error'?'errorMessage':'muted'));line();}
        if(!state.messages?.length)line('No messages yet. Write below to start this conversation.','muted');
      } else if(page==='loading'){heading('Loading…');line('Connecting to your workspace.','muted');}
      else if(page==='error'){heading('Workspace unavailable');line('Could not load this page. Your draft is preserved.','muted');line('[Retry]','link','scenario:home');}
      return lines;
    }
    function drawSearch(x,y,w,h) {
      fill(x,y,w,1,'searchInput');
      put(x+1,y,clip(state.query||'',w-2),'searchInput','search:input');
      for(let col=x;col<x+w;col++)grid[y][col].action='search:input';
      regions.searchInput={x:x+1,y,width:w-2,height:1};
      const results=searchResults(state),limit=state.searchLimit||50;
      const options=results.slice(0,limit),more=results.length>limit;
      const selected=Math.min(state.searchSelection??-1,options.length+(more?1:0)-1);
      const height=Math.max(0,h-2),count=options.length+(more?1:0);
      const offset=Math.max(0,Math.min(state.mainScroll||0,Math.max(0,count-height)));
      for(let i=offset;i<Math.min(count,offset+height);i++) {
        const yy=y+2+i-offset,face=selected===i?'selected':'base';
        fill(x,yy,w,1,face);
        const action=i===options.length?'search:more':`search:select:${i}`;
        if(i===options.length)put(x+1,yy,'[Show 50 more]',face,action);
        else {
          const chat=options[i],end=badge(x+1,yy,chat,false,face);
          put(end+2,yy,clip(chat.title,w-(end-x)-3,true),face,action);
        }
        for(let col=x;col<x+w;col++)grid[yy][col].action=action;
      }
      if(!count)put(x+1,y+2,state.searchLoading?'Searching…':'No matching chats.','muted');
      if(state.focus==='main')cursor=selected<0
        ?{row:y,col:x+1+Math.min(textWidth((state.query||'').slice(0,state.searchPoint??(state.query||'').length)),w-2)}
        :{row:y+2+Math.max(0,Math.min(height-1,selected-offset)),col:x+1};
    }
    function drawMain(page,chat,x,y,w,h,scroll=0) {
      if(page==='search'){drawSearch(x,y,w,h);return;}
      if(page==='chat' && chat) {
        const end=badge(x,y,chat,true,'base',true);
        put(end+2,y,clip(chat.title,w-(end-x)-3,true),'orange');
        let col=x;
        const seconds=chat.elapsed_seconds||0,tokens=chat.tokens_used||0;
        const values=[['goal',chat.goal_enabled===false?'off':chat.goal?.status||'ready'],
          ['tokens',tokens<1000?String(tokens):`${Math.floor(tokens/1000)}k`],
          ['elapsed',seconds<60?`${Math.floor(seconds)}s`:seconds<3600?`${Math.floor(seconds/60)}m`:`${Math.floor(seconds/3600)}h`]];
        const tail=values.reduce((n,[label,value])=>n+label.length+value.length+5,0);
        values.unshift(['project',clip(chat.project||'—',Math.max(1,w-1-tail-11),true)]);
        values.forEach(([label,value],index)=>{
          if(index)col=put(col,y+1,' ');
          col=put(col,y+1,` ${label} `,'location');col=put(col,y+1,` ${value} `,'metadataValue');
        });
        y+=2;h-=2;
      }
      const lines=pageLines(page,chat,w);const offset=Math.max(0,Math.min(scroll,Math.max(0,lines.length-h)));
      lines.slice(offset,offset+h).forEach((line,row)=>{
        const yy=y+row;
        if(line.chat) {
          if(line.row && line.chat.active)fill(x,yy,w,1,line.chat.kind==='bot'?'bot':'manager');
          const rowFace=line.row && line.chat.active?(line.chat.kind==='bot'?'bot':'manager'):'base';
          const end=badge(x+1,yy,line.chat,!line.row,rowFace,!line.row);
          if(line.title||line.row)put(end+2,yy,clip(line.chat.title,w-(end-x)-3,true),line.title?'orange':rowFace,`chat:${line.chat.id}`);
          if(line.row)for(let xx=x;xx<x+w;xx++)grid[yy][xx].action=`chat:${line.chat.id}`;
        } else if(line.metadata) {
          let col=x+1;
          for(const cell of line.cells){put(col,yy,cell.ch,cell.face);col+=width(cell.ch);}
        } else if(line.internalBadge) {put(x+1,yy,` ${line.internalBadge} `,'internalBadge');
        } else if(line.userBadge) {put(x+1,yy,' user ','userBadge');}
        else if(line.key) {put(x+1,yy,line.key.padEnd(20),'key');put(x+23,yy,clip(line.description,w-24));}
        else if(line.message) {
          fill(x+1,yy,w-1,1,line.face);put(x+1,yy,'▏',withBackground(line.role==='internal'?'muted':line.role==='user'?'blue':line.role==='bot'?'purple':'orange',line.face));
          let col=x+3;
          for(const cell of line.cells){put(col,yy,cell.ch,cell.face,cell.action);col+=width(cell.ch);}
        } else {
          const margin=['file','project-readme'].includes(page)?0:1;
          if(line.face==='errorMessage')fill(x+margin,yy,w-margin,1,line.face);
          put(x+margin,yy,clip(line.text,w-margin-1),line.face,line.action);
          if(!margin && textWidth(line.text)>w-1)put(x+w-1,yy,'$','base');
        }
      });
      if(offset+ h<lines.length)put(x+w-2,y+h-1,'↓','muted','main:down');
      if(offset>0&&page!=='chat')put(x+w-2,y,'↑','muted','main:up');
    }
    if(state.mode==='cowork' && state.split) {
      const other=chatById(state.other)||visibleChats[1]||current;
      if(state.split==='right') {
        const half=Math.floor(columns/2);
        drawMain('chat',current,0,0,half,contentHeight);
        for(let y=0;y<contentHeight;y++)put(half,y,'│','rule');
        drawMain(state.otherPage||'chat',other,half+1,0,columns-half-1,contentHeight);
      } else {
        const half=Math.floor(contentHeight/2);
        drawMain('chat',current,0,0,columns,half);
        put(0,half,'─'.repeat(columns),'rule');
        drawMain(state.otherPage||'chat',other,0,half+1,columns,contentHeight-half-1);
      }
    } else drawMain(state.page||'home',current,mainX,0,mainWidth,contentHeight,state.mainScroll||0);
    if(composerVisible) {
      const top=contentHeight;
      const tabrow=(kind,y,prefix,start)=>{
        const face=kind==='bot'?'purpleBar':'orangeBar';fill(0,y,columns,1,face);
        const x=10;
        if(kind==='manager')put(0,y,' M-0 new ',withBackground('managerShortcut',face),'new-manager');
        const chats=visibleChats.filter(chat=>chat.kind===kind).slice(0,kind==='bot'?9:10);
        const offset=kind==='bot'?(state.botTabScroll||0):(state.managerTabScroll||0);
        let xx=x;
        chats.forEach((chat,index)=>{
          if(index<offset)return;
          const selected=chat.id===state.target;
          const tabFace=withBackground(kind==='bot'?'botShortcut':'managerShortcut',selected?(kind==='bot'?'botBadge':'managerBadge'):face);
          const circle=chat.active||['error','interrupted','cancelled','blocked'].includes(chat.status);
          const leading=' ';
          const label=leading+clip(`${circle?'● ':''}${prefix}${index+start} ${chat.title}`,18,true)+' ';
          put(xx,y,label,tabFace,`target:${chat.id}`);
          put(xx+leading.length+(circle?2:0),y,`${prefix}${index+start}`,selected?tabFace:withBackground(kind==='bot'?'botShortcut':'managerShortcut',tabFace),`target:${chat.id}`);
          if(circle)put(xx+leading.length,y,'●',withBackground(chat.active?(selected?'green':'inactiveGreen'):'error',tabFace),`target:${chat.id}`);
          xx+=textWidth(label);
        });
        put(columns-1,y,' ',face);
        if(xx>columns-1)put(columns-2,y,'›',face,`tabs:${kind}`);
      };
      tabrow('bot',top,'M-',1);tabrow('manager',top+1,'cb',0);
      let labelEnd=1;
      if(target){labelEnd=badge(1,top+2,target);put(labelEnd+2,top+2,clip(`continue '${target.title}'`,columns-labelEnd-3),'muted');}
      else {put(1,top+2,' m ','managerBadge');put(6,top+2,'New chat','muted');}
      let editY=top+3;
      if(attached.length){put(1,editY++,'Attachments','muted');attached.forEach((attachment,index)=>{
        put(1,editY,'[del]','link',`attachment:${index}`);put(7,editY,attachment.name,'link','scenario:link-media');
        put(9+textWidth(attachment.name),editY,attachment.size,'muted');editY++;
      });}
      const face=target?.kind==='bot'?'botInput':'managerInput';fill(0,editY,columns,inputRows,face);
      const offset=Math.max(0,draftLayout.cursor.row-inputRows+1);
      draftLayout.lines.slice(offset,offset+inputRows).forEach((line,index)=>put(1,editY+index,line,face));
      regions.input={x:1,y:editY,width:columns-2,height:inputRows};
      regions.bars={x:0,y:top,width:columns,height:3};
      if((state.focus||'input')==='input')cursor={row:editY+draftLayout.cursor.row-offset,col:1+draftLayout.cursor.col};
      if(offset>0)put(columns-2,editY,'↑','muted');
    }
    if(state.focus==='sidebar')cursor={row:state.focusRow||2,col:1};
    if(state.focus==='main' && state.page!=='search')cursor={row:state.focusRow||2,col:mainX+(['file','project-readme'].includes(state.page)?0:1)};
    if(state.selection) {
      const y=state.selection.row, x=mainX+3;
      for(let i=x;i<Math.min(columns-1,x+state.selection.length);i++)if(grid[y]?.[i])grid[y][i].face='selection';
      cursor={row:y,col:x};
    }
    fill(0,rows-1,columns,1,'echo');
    const echo=state.echo || '';
    put(0,rows-1,clip(echo,columns),state.failure?'errorMessage':'echo');
    if(cursor && grid[cursor.row]?.[cursor.col])grid[cursor.row][cursor.col].face='cursor';
    const lines=grid.map(row=>row.map(cell=>cell.ch).join(''));
    const runs=grid.map(row=>{
      const result=[];let current=null;
      row.forEach((cell,col)=>{if(current&&current.face===cell.face&&current.action===cell.action)current.text+=cell.ch;
        else{current={col,text:cell.ch,face:cell.face,...(cell.action?{action:cell.action}:{})};result.push(current);}});
      return result;
    });
    return {columns,rows,lines,runs,regions,cursor,palette};
  }
  root.AiboTerminal={render,palette,textWidth,inputLines,searchResults};
})(globalThis);
