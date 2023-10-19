
    function
    the_function_that_runs_in_the_context_of_gb_dot_html
        (gb_title    : string,
         gb_miscinfo : string,
         gb_timeout_ms  : number)
        : string
    {
        // FIXME: not alerting user about bobs
        alert('bobs!');
        //// the last evaluated statement of this function is the "return value"
        //// as far as scripting.executeScript is concerned
        //console.log('gb_title', gb_title);
        //console.log('gb_miscinfo', gb_miscinfo);
        //console.log('gb_timeout_ms', gb_timeout_ms);

        //document.getElementById('title-h1')!.innerHTML = gb_title;

        return 'good';
    }

    console.log('badness');
    // @ts-ignore i'm just assuming typescript is going to be stupid here i don't even know
    let dialog_result: Array<{result: gb} | {error: any}> =
            await browser.scripting.executeScript(
                // @ts-ignore i'm just assuming typescript is going to be stupid here i don't even know
                {func   : the_function_that_runs_in_the_context_of_gb_dot_html,
                 args   : [title, miscinfo, timeout_ms],
                 target : {tabId : tabid},
                 // script just hangs forever without this line
                 // in no case does it alert user about bobs
                 // alright
                 // need a break
                 // when we get back, let's try a file
                 // otherwise we're going to have to try a totally different idiom
                 // maybe go back to some retarded message passing nonsense
                 injectImmediately: true
                 }
            );

    console.log('dialog_result', dialog_result[0]);
    // @ts-ignore
    if (dialog_result[0].error) {
        // @ts-ignore
        throw dialog_result.error;
    }
    else {
        // @ts-ignore
        return dialog_result[0].result;
    }
