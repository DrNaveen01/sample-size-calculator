const {chromium}=require('playwright');
const fs=require('fs');
fs.mkdirSync('output/browser',{recursive:true});
(async()=>{
const browser=await chromium.launch({headless:true,...(process.env.CHROME_EXECUTABLE ? {executablePath:process.env.CHROME_EXECUTABLE} : {}),args:['--no-sandbox']});
const page=await browser.newPage({viewport:{width:1440,height:1050},acceptDownloads:true});
const errors=[];page.on('pageerror',e=>errors.push(String(e)));
await page.goto(process.env.APP_URL || 'http://127.0.0.1:8766',{waitUntil:'domcontentloaded',timeout:20000});
await page.waitForFunction(()=>window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket.readyState===1);
async function settle(){await page.waitForTimeout(1200);await page.waitForFunction(()=>!document.querySelector('.shiny-busy'));}
async function select(id,v){await page.locator('#'+id).selectOption(v);await settle();}
async function fill(id,v){await page.locator('#'+id).fill(String(v));await page.locator('#'+id).press('Tab');await settle();}
await settle();
const checks=[];
for(const calc of ['single_proportion','single_mean','two_proportions','two_means','yamane','auc','diagnostic','correlation','effects']){
await select('calculator',calc);
const labels={single_proportion:'single population proportion',single_mean:'single population mean',two_proportions:'two independent proportions',two_means:'two independent means',yamane:'Taro Yamane',auc:'ROC AUC',diagnostic:'Diagnostic accuracy',correlation:'Pearson correlation',effects:'Effect measures'};
await page.waitForFunction(label=>document.querySelector('.calculation-report')?.textContent.includes(label),labels[calc]);
await settle();
const error=await page.locator('.error-panel').count();if(error)throw Error(calc+': '+await page.locator('.error-panel').innerText());
checks.push({calculator:calc,result:await page.locator('.hero-number').innerText()});
await page.screenshot({path:'output/browser/ui-'+calc+'.png'});
}
await select('calculator','two_means');
for(const objective of ['equality','superiority','noninferiority','equivalence']){
await select('objective',objective);
if(objective==='equivalence') await fill('mean2',101);
await page.waitForFunction(label=>document.querySelector('.objective-label')?.textContent.includes(label),objective==='noninferiority'?'Non-inferiority':objective==='equivalence'?'Equivalence':objective==='superiority'?'Superiority':'Equality');
await settle();
checks.push({trial:objective,result:await page.locator('.hero-number').innerText()});
}
await page.locator('a[data-value="plots"]').click();await settle();
await page.screenshot({path:'output/browser/ui-trial-plots.png'});
await select('calculator','auc');await select('auc_objective','paired');await fill('auc_rho',.6);
await page.locator('a[data-value="report"]').click();await settle();
await fill('auc_rho',1);if(!await page.locator('.error-panel').count())throw Error('Invalid rho did not clear results');
if((await page.locator('#markdown_text').inputValue()).length)throw Error('Stale Markdown after invalid rho');
await fill('auc_rho',.5);
await select('advanced_mode','power');await fill('auc_cases',200);await fill('auc_controls',400);
checks.push({aucPower:await page.locator('.hero-number').innerText()});
await select('calculator','diagnostic');await select('diag_objective','joint');await select('advanced_mode','sample_size');
await page.locator('a[data-value="report"]').click();await settle();
const downloadPromise=page.waitForEvent('download');await page.locator('#download_docx').click();const dl=await downloadPromise;await dl.saveAs('output/browser/browser-diagnostic.docx');
await select('calculator','correlation');await select('corr_objective','estimate');
await page.screenshot({path:'output/browser/ui-correlation-estimate.png'});
await select('calculator','effects');await select('effects_design','case_control');
const report=await page.locator('#markdown_text').inputValue();if(report.includes('| Risk ratio |'))throw Error('Case-control risk measures leaked');
await page.setViewportSize({width:390,height:844});await page.screenshot({path:'output/browser/ui-mobile.png',fullPage:true});
const overflow=await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth);if(overflow)throw Error('Mobile horizontal overflow');
await page.setViewportSize({width:1440,height:1050});await select('calculator','two_proportions');
const before=await page.evaluate(()=>({a:document.querySelector('.input-panel').scrollTop,b:document.querySelector('.results-panel').scrollTop,y:scrollY}));
await page.locator('.input-panel').hover();await page.mouse.wheel(0,500);await page.waitForTimeout(200);
const after=await page.evaluate(()=>({a:document.querySelector('.input-panel').scrollTop,b:document.querySelector('.results-panel').scrollTop,y:scrollY}));
if(after.a<=before.a||after.b!==before.b||after.y!==before.y)throw Error('Independent input scrolling failed');
fs.writeFileSync('output/browser/browser-results.json',JSON.stringify({checks,errors,overflow,scroll:{before,after}},null,2));
console.log(JSON.stringify({checks,errors,overflow,scroll:{before,after}}));
await browser.close();
})().catch(e=>{console.error(e);process.exit(1)});
