import * as Pdf from "jspdf";

type Settings = {
    offsetX: number,
    offsetY: number,
    cardWidth: number,
    cardHeight: number,
    positions: Array<Point>,
    cardsPerPage: number
}

type Row = Array<string>
type Card = Array<Row>
type Cards = Array<Card>

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

function drawCard({ x, y }: Point, card: Card, pdf: Pdf, n: number): Pdf {
    const realX = x + settings.offsetX;
    const realY = y + settings.offsetY;
    const width = card.length;
    const height = card[0].length;
    const cellWidth = settings.cardWidth / width;
    const cellHeight = settings.cardHeight / (height + 1);

    // borders + header
    pdf.setLineWidth(2);
    //pdf.setFillColor("#FAFAFA");
    pdf.rect(realX, realY, settings.cardWidth, settings.cardHeight);
    card.forEach((column: string[], i: number) => {
        const fromLeft = realX + i * cellWidth;

        // outer lines
        pdf.setLineWidth(1);
        pdf.rect(fromLeft, realY, cellWidth, cellHeight)

        // text-header-row
        pdf.setFontSize(28);
        const letter = "BINGO".charAt(i % width);
        const { w, h } = pdf.getTextDimensions(letter);
        pdf.text(letter, fromLeft + cellWidth / 2 - w / 2, realY + cellHeight / 2 + h / 2);

        // rest of the card
        pdf.setLineWidth(.5);
        column.forEach((_, j) => {
            const fromTop = realY + (j + 1) * cellHeight;

            // cells
            pdf.rect(fromLeft, fromTop, cellWidth, cellHeight);

            // text in cell
            pdf.setFontSize(18);
            const text = card[i][j];
            const { w, h } = pdf.getTextDimensions(text);
            pdf.text(card[i][j], fromLeft + cellWidth / 2 - w / 2, fromTop + cellHeight / 2 + h / 2);

        });
    });
    pdf.setFontSize(6);
    pdf.text(n + "", realX + settings.cardWidth - 4, realY + settings.cardHeight + 3);

    return pdf;
}

export function create(cards: Cards): void {
    let pdf = new Pdf({
        orientation: "landscape"
    });

    cards.forEach((card: Card, i: number) => {
        const n = i % settings.cardsPerPage;
        const position = settings.positions[n];
        if (i !== 0 && i % settings.cardsPerPage === 0) {
            pdf.addPage("a4", "landscape");
        }
        pdf = drawCard(position, card, pdf, i + 1);


    })

    pdf.save("bingo-cards.pdf");

}