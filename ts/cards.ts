import * as Pdf from "jspdf";

type Settings = {
    offsetX: number,
    offsetY: number,
    cardWidth: number,
    cardHeight: number,
    positions: Array<Point>,
    cardsPerPage: number
}

export type Row = Array<string>
export type Card = Array<Row>
export type Cards = Array<Card>

type Point = {
    x: number,
    y: number
};

const point = (x: number, y: number): Point => ({ x, y })

const settings: Settings = {
    offsetX: 10,
    offsetY: 10,
    cardWidth: 133,
    cardHeight: 90,
    positions: [point(0, 0), point(145, 0), point(0, 100), point(145, 100)],
    cardsPerPage: 4
}

const getImageData = (svgStr: string): HTMLCanvasElement => {
    const image = document.createElement("img");
    image.src = `data:image/svg+xml;${svgStr}`;
    image.width = 50;
    image.height = 50;

    const canvas = document.createElement("canvas");
    canvas.width = 50;
    canvas.height = 50;

    const context = canvas.getContext("2d", { alpha: true });
    if (context === null) {
        throw new Error("Upsie!");
    }
    else {
        context.fillStyle = "#rgb(255,255,255)"
        context.fillRect(0, 0, 50, 50);
        context.drawImage(image, 0, 0);
    }

    //const container = document.createElement("div");
    //container.innerHTML = svgStr;
    //document.body.appendChild(container);
    return canvas;
};

const ptToMm = (pt: number): number => pt / (72 / 25.6);
const mmToPt = (mm: number): number => mm * (72 / 25.6);

const percentOf = (p: number, x: number): number =>
    (p / 100) * x;

const getFontSize = (width: number, height: number, card: Card, pdf: Pdf): number => {
    const longestStr = card.reduce(
        (longStr, arr) => {
            const str = arr.reduce(
                (s, v) =>
                    v !== "%_JOKER_%" && v.length > s.length
                        ? v
                        : s,
                ""
            );
            return str.length > longStr.length
                ? str
                : longStr;
        },
        ""
    );
    const unitWidth = pdf.getStringUnitWidth(longestStr);

    let fontSize = mmToPt(width) / unitWidth;
    //console.log(unitWidth, fontSize, ptToMm(fontSize));

    while (ptToMm(fontSize) > height) {
        fontSize -= 1;
    }
    return fontSize;
};

// i will think of something better
const headerLetter = (i: number): string => {
    return ("BINGO")[i % 5];
};

const drawCard = ({ x, y }: Point, card: Card, pdf: Pdf, n: number, jokerSvgStr: string): Pdf => {
    const realX = x + settings.offsetX;
    const realY = y + settings.offsetY;
    const width = card.length;
    const height = card[0].length;
    const cellWidth = settings.cardWidth / width;
    const cellHeight = settings.cardHeight / (height + 1);
    const headerFontSize = mmToPt(percentOf(80, cellHeight));
    const fontSize = getFontSize(percentOf(90, cellWidth), percentOf(75, cellHeight), card, pdf);

    const joker = getImageData(jokerSvgStr);

    // borders + header
    pdf.setLineWidth(2);
    pdf.rect(realX, realY, settings.cardWidth, settings.cardHeight);
    card.forEach((column: string[], i: number) => {
        const fromLeft = realX + i * cellWidth;


        // outer lines
        pdf.setLineWidth(1);
        pdf.rect(fromLeft, realY, cellWidth, cellHeight)

        // text-header-row
        pdf.setFontSize(headerFontSize);
        pdf.setFontStyle("bold");
        const letter = headerLetter(i);
        const { w, h } = pdf.getTextDimensions(letter);
        pdf.text(letter, fromLeft + cellWidth / 2 - w / 2, realY + cellHeight / 2 + h / 2);

        // rest of the card
        pdf.setLineWidth(.5);
        pdf.setFontSize(fontSize);
        pdf.setFontStyle("normal");
        column.forEach((_, j) => {
            const fromTop = realY + (j + 1) * cellHeight;

            // cells
            pdf.rect(fromLeft, fromTop, cellWidth, cellHeight);

            // text in cell
            const text = card[i][j];


            if (text === "%_JOKER_%") {
                const imgX = fromLeft + (cellWidth - cellHeight) / 2;
                const imgY = fromTop;
                pdf.addImage(joker, imgX, imgY, cellHeight, cellHeight)
            }
            else {
                const { w, h } = pdf.getTextDimensions(text);
                const x = fromLeft + (cellWidth - w) / 2;
                const y = fromTop + (cellHeight + h) / 2;
                pdf.text(text, x, y);
            }

        });
    });
    pdf.setFontSize(6);
    pdf.text(n + "", realX + settings.cardWidth - 4, realY + settings.cardHeight + 3);

    return pdf;
};

export const create = (cards: Cards, svgData: string): Pdf => {
    let pdf = new Pdf({
        orientation: "landscape"
    });

    pdf.setFontType("courier");
    //console.log(pdf.getFontList());

    cards.forEach((card: Card, i: number) => {
        const n = i % settings.cardsPerPage;
        const position = settings.positions[n];
        if (i !== 0 && i % settings.cardsPerPage === 0) {
            pdf.addPage("a4", "landscape");
        }
        pdf = drawCard(position, card, pdf, i + 1, svgData);

    });

    return pdf;
};