export function play_safe(fileName) {
    try {
        let audio = new Audio(fileName);
        let playPromise = audio.play();

        if (playPromise !== undefined) {
            playPromise.then(_ => { })
                .catch(error => {
                    console.log("play failed: ", error);
                });
        }
    } catch (ex) {
        const arg = ex.message;
        console.log("play error: ", arg);
    }
}